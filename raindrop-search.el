;;; raindrop-search.el --- As-you-type Raindrop search completion  -*- lexical-binding: t; -*-

;; Author: artawower <artawower33@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, matching
;; URL: https://github.com/your/repo

;;; Commentary:
;; Interactive search interface for Raindrop bookmarks with:
;; - As-you-type completion with animated spinner
;; - Smart collection/tag parsing (#tag, [folder])
;; - Embark integration for editing/deleting items
;; - Customizable spinner animation and appearance

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'cl-lib)
(require 'raindrop)

(declare-function raindrop--kv "raindrop" (item key))
(declare-function raindrop--collection-id-by-title "raindrop" (title))
(declare-function raindrop--collection-title-by-id "raindrop" (id))
(declare-function raindrop--ensure-collections-async "raindrop" (callback))
(declare-function raindrop--parse-search-input "raindrop" (input))
(declare-function raindrop--meaningful-search-input-p "raindrop" (parsed))
(declare-function raindrop-search-bookmarks "raindrop" (input callback &optional page-size page))
(declare-function raindrop-api-request-async "raindrop" (endpoint method params payload callback))

(defgroup raindrop-search nil
  "Completion UI for Raindrop."
  :group 'convenience)

(defun raindrop-search--debug (fmt &rest args)
  "Log debug message if `raindrop-debug' is enabled."
  (when (bound-and-true-p raindrop-debug)
    (message "[raindrop-search] %s" (apply #'format fmt args))))

(defface raindrop-search-tag
  '((t :inherit font-lock-keyword-face :foreground "DodgerBlue3"))
  "Face for tags in completion results."
  :group 'raindrop-search)

(defface raindrop-search-collection
  '((t :inherit font-lock-function-name-face :foreground "orange"))
  "Face for collections in completion results."
  :group 'raindrop-search)

(defface raindrop-search-edit-key
  '((t :inherit font-lock-keyword-face))
  "Face used for read-only field labels in the edit buffer."
  :group 'raindrop-search)

(defface raindrop-search-edit-header
  '((t :inherit shadow :weight normal))
  "Face used for the header line in the edit buffer."
  :group 'raindrop-search)

(defcustom raindrop-search-title-max 20
  "Maximum characters for titles before truncation."
  :type 'integer
  :group 'raindrop-search)

(defcustom raindrop-search-note-max 20
  "Maximum characters for notes before truncation."
  :type 'integer
  :group 'raindrop-search)

(defcustom raindrop-search-excerpt-max 40
  "Maximum characters for excerpts before truncation."
  :type 'integer
  :group 'raindrop-search)

(defcustom raindrop-search-idle-delay 0.25
  "Idle time before firing a Raindrop API request."
  :type 'number
  :group 'raindrop-search)

(defcustom raindrop-search-page-size 50
  "Number of items to request per page from the Raindrop API (1..50)."
  :type 'integer
  :group 'raindrop-search)

(defcustom raindrop-search-text-collection 0
  "Default collection ID to search when no [collection] token is used.
0 means the root (\"All\"), while -1 refers to \"Unsorted\"."
  :type 'integer
  :group 'raindrop-search)

(defcustom raindrop-search-enter-action 'link
  "Enter opens either original link or Raindrop app."
  :type '(choice (const link) (const raindrop))
  :group 'raindrop-search)

(defcustom raindrop-search-raindrop-url-builder
  #'raindrop-search--default-raindrop-url-builder
  "Function used to build a Raindrop app URL for ITEM.
Called with a normalized Raindrop item plist/alist and must return a
URL string."
  :type 'function
  :group 'raindrop-search)

(defcustom raindrop-search-spinner-frames '("|" "/" "-" "\\")
  "Frames for the loading spinner animation."
  :type '(repeat string)
  :group 'raindrop-search)

(defcustom raindrop-search-spinner-delay 0.1
  "Delay in seconds between spinner frame updates."
  :type 'number
  :group 'raindrop-search)

(defvar raindrop-search--timer nil
  "Timer for delayed API requests.")

(defvar raindrop-search--last-gen 0
  "Generation counter for tracking request versions.")

(defvar raindrop-search--items nil
  "Current list of search result items.")

(defvar raindrop-search--last-input nil
  "Last input string to detect changes.")

(defvar raindrop-search--cand-map (make-hash-table :test 'equal)
  "Hash table mapping candidate strings to items.")

(defvar raindrop-search--loading nil
  "Whether a search is currently loading.")

(defvar raindrop-search--spinner-timer nil
  "Timer for spinner animation.")

(defvar raindrop-search--spinner-index 0
  "Current frame index in spinner animation.")

(defvar-local raindrop-search--edit-item-id nil
  "ID of the item being edited in the current buffer.")


(defun raindrop-search--start-spinner ()
  "Start the loading spinner animation."
  (setq raindrop-search--loading t
        raindrop-search--spinner-index 0)
  (when raindrop-search--spinner-timer
    (cancel-timer raindrop-search--spinner-timer))
  (setq raindrop-search--spinner-timer
        (run-with-timer 0 raindrop-search-spinner-delay #'raindrop-search--update-spinner)))

(defun raindrop-search--stop-spinner ()
  "Stop the loading spinner animation."
  (setq raindrop-search--loading nil)
  (when raindrop-search--spinner-timer
    (cancel-timer raindrop-search--spinner-timer)
    (setq raindrop-search--spinner-timer nil))
  (raindrop-search--safe-exhibit))

(defun raindrop-search--update-spinner ()
  "Update spinner frame and refresh display."
  (when raindrop-search--loading
    (setq raindrop-search--spinner-index
          (mod (1+ raindrop-search--spinner-index)
               (length raindrop-search-spinner-frames)))
    (raindrop-search--safe-exhibit)))

(defun raindrop-search--get-spinner-frame ()
  "Get current spinner frame."
  (if raindrop-search--loading
      (nth raindrop-search--spinner-index raindrop-search-spinner-frames)
    ""))

(defun raindrop-search--truncate (s n)
  "Truncate string S to N characters with ellipsis."
  (if (and (stringp s) (> (length s) n))
      (concat (substring s 0 n) "…")
    (or s "")))

(defalias 'raindrop-search--kv 'raindrop--kv)

(defun raindrop-search--safe-exhibit ()
  "Force completion UI update across different frameworks."
  (let ((win (active-minibuffer-window))
        (gen raindrop-search--last-gen))
    (when (and win (minibufferp (window-buffer win)))
      (with-selected-window win
        (when (= gen raindrop-search--last-gen)
          (cond
           ((and (boundp 'vertico-mode) vertico-mode)
            (with-current-buffer (window-buffer win)
              (when (boundp 'vertico--input)
                (setq vertico--input nil))
              (when (boundp 'vertico--history-hash)
                (setq vertico--history-hash nil))
              (when (boundp 'vertico--lock-candidate)
                (setq vertico--lock-candidate nil))
              (when (fboundp 'vertico--update)
                (vertico--update))
              (let ((content (minibuffer-contents-no-properties)))
                (delete-minibuffer-contents)
                (insert content))
              (when (fboundp 'vertico--exhibit)
                (vertico--exhibit))
              (redisplay t)))
           ((and (boundp 'ivy-mode) ivy-mode (fboundp 'ivy--exhibit))
            (ivy--exhibit))
           ((and (boundp 'icomplete-mode) icomplete-mode)
            (when (fboundp 'icomplete-exhibit)
              (icomplete-exhibit))
            (redisplay t))
           ((fboundp 'mct--exhibit)
            (mct--exhibit))
           ((and (boundp 'selectrum-mode) selectrum-mode (fboundp 'selectrum--update))
            (selectrum--update))
           (t (redisplay t))))))))

(defalias 'raindrop-search--collection-id-by-title 'raindrop--collection-id-by-title)
(defalias 'raindrop-search--collection-title-by-id 'raindrop--collection-title-by-id)

(defun raindrop-search--ensure-collections ()
  "Ensure collections are loaded, refreshing UI when ready."
  (raindrop--ensure-collections-async
   (lambda () (raindrop-search--safe-exhibit))))

(defalias 'raindrop-search--parse 'raindrop--parse-search-input)
(defalias 'raindrop-search--meaningful-input-p 'raindrop--meaningful-search-input-p)

(defun raindrop-search--domain-of (link item)
  "Extract domain from LINK or ITEM."
  (or (raindrop-search--kv item 'domain)
      (and (stringp link)
           (when (string-match "//\KATEX_INLINE_OPEN[^/]+\KATEX_INLINE_CLOSE" link)
             (match-string 1 link)))))

(defun raindrop-search--tags->strings (tags)
  "Convert TAGS to list of lowercase strings."
  (cond
   ((null tags) nil)
   ((vectorp tags) (mapcar (lambda (x) (downcase (format "%s" x))) (append tags nil)))
   ((listp tags)   (mapcar (lambda (x) (downcase (format "%s" x))) tags))
   (t nil)))

(defun raindrop-search--format-candidate (it)
  "Format IT as a completion candidate string. Title (if any) before excerpt."
  (let* ((raw-title   (or (raindrop-search--kv it :title)
                          (raindrop-search--kv it 'title) ""))
         (raw-excerpt (or (raindrop-search--kv it :excerpt)
                          (raindrop-search--kv it 'excerpt) ""))
         (t0 (string-trim (or raw-title "")))
         (e0 (string-trim (or raw-excerpt "")))
         (title* (and (not (string-empty-p t0))
                      (raindrop-search--truncate t0 raindrop-search-title-max)))
         (desc*  (and (not (string-empty-p e0))
                      (not (string= e0 t0))
                      (raindrop-search--truncate e0 raindrop-search-excerpt-max)))
         (link    (or (raindrop-search--kv it :link)
                      (raindrop-search--kv it 'link) ""))
         (note    (if-let ((note (string-trim
                                  (or (raindrop-search--kv it :note)
                                      (raindrop-search--kv it 'note)))))
                      (and (not (string-empty-p note))
                           (format "%s %s" (propertize "Note:" 'face 'bold)
                                   (raindrop-search--truncate note raindrop-search-note-max)))
                    nil))
         (domain* (and-let* ((d (raindrop-search--domain-of link it)))
                    (propertize d 'face 'shadow)))
         (tags    (raindrop-search--tags->strings
                   (or (raindrop-search--kv it :tags)
                       (raindrop-search--kv it 'tags))))
         (tagstr  (when (and tags (listp tags) tags)
                    (mapconcat
                     (lambda (tag) (propertize (format "#%s" tag) 'face 'raindrop-search-tag))
                     tags " ")))
         (coll-obj (or (raindrop-search--kv it :collection)
                       (raindrop-search--kv it 'collection)))
         (coll-id  (or (raindrop-search--kv it :collectionId)
                       (raindrop-search--kv it 'collectionId)
                       (and (consp coll-obj)
                            (or (raindrop-search--kv coll-obj 'id)
                                (raindrop-search--kv coll-obj :id)
                                (raindrop-search--kv coll-obj '_id)
                                (raindrop-search--kv coll-obj :_id)
                                (raindrop-search--kv coll-obj '$id)
                                (raindrop-search--kv coll-obj :$id)
                                (raindrop-search--kv coll-obj 'oid)
                                (raindrop-search--kv coll-obj :oid)))))
         (coll-title
          (or (and (consp coll-obj)
                   (or (raindrop-search--kv coll-obj :title)
                       (raindrop-search--kv coll-obj 'title)
                       (raindrop-search--kv coll-obj :name)
                       (raindrop-search--kv coll-obj 'name)))
              (and (integerp coll-id)
                   (raindrop-search--collection-title-by-id coll-id))))
         (collstr (when coll-title
                    (propertize (format "[%s]" coll-title)
                                'face 'raindrop-search-collection))))
    (string-join (delq nil (list collstr title* desc* tagstr domain* note)) "  ")))

(defun raindrop-search--apply-results (gen items)
  "Apply ITEMS results if GEN matches current generation."
  (when (= gen raindrop-search--last-gen)
    (raindrop-search--stop-spinner)
    (setq raindrop-search--items items)
    (raindrop-search--safe-exhibit)))

(defun raindrop-search--fetch (gen page parsed)
  "Fetch results for GEN at PAGE using PARSED input."
  (let* ((tags (plist-get parsed :tags))
         (excluded-tags (plist-get parsed :excluded-tags))
         (folders (plist-get parsed :folders))
         (text (plist-get parsed :text))
         (input (string-join
                 (append
                  (mapcar (lambda (tag) (raindrop--quote-tag tag)) tags)
                  (mapcar (lambda (tag) (concat "-" (raindrop--quote-tag tag))) excluded-tags)
                  (mapcar (lambda (folder) (concat "[" folder "]")) folders)
                  (and text (not (string-empty-p text)) (list text)))
                 " ")))
    (raindrop-search--debug "fetch(gen=%s) input=%S" gen input)
    (raindrop-search-bookmarks
     input
     (lambda (items err)
       (when (= gen raindrop-search--last-gen)
         (if err
             (progn (message "Raindrop error: %s" err)
                    (raindrop-search--apply-results gen nil))
           (raindrop-search--apply-results gen items))))
     raindrop-search-page-size page)))

(defun raindrop-search--idle-fire ()
  "Fire API request after idle delay."
  (let* ((input (minibuffer-contents-no-properties))
         (parsed (raindrop-search--parse input)))
    (when (and (equal input raindrop-search--last-input)
               (raindrop-search--meaningful-input-p parsed))
      (raindrop-search--debug "idle fire input=%S" input)
      (raindrop-search--start-spinner)
      (setq raindrop-search--last-gen (1+ raindrop-search--last-gen))
      (raindrop-search--fetch raindrop-search--last-gen 0 parsed))))

(defun raindrop-search--schedule ()
  "Schedule API request only if input actually changed."
  (let ((input (minibuffer-contents-no-properties)))
    (cond
     ((equal input raindrop-search--last-input)
      nil)
     (t
      (setq raindrop-search--last-input input)
      (setq raindrop-search--items nil)
      (raindrop-search--safe-exhibit)
      (setq raindrop-search--last-gen (1+ raindrop-search--last-gen))
      (when raindrop-search--timer
        (cancel-timer raindrop-search--timer)
        (setq raindrop-search--timer nil))
      (let ((parsed (raindrop-search--parse input)))
        (when (raindrop-search--meaningful-input-p parsed)
          (setq raindrop-search--timer
                (run-with-idle-timer raindrop-search-idle-delay nil
                                     #'raindrop-search--idle-fire))))))))

(defun raindrop-search--ui-candidates ()
  "Generate completion candidates for current state."
  (setq raindrop-search--cand-map (make-hash-table :test 'equal))
  (cond
   (raindrop-search--loading
    (let ((spinner (raindrop-search--get-spinner-frame)))
      (list (propertize (format "%s Loading..." spinner)
                        'face 'shadow))))
   (raindrop-search--items
    (let ((i -1))
      (mapcar
       (lambda (it)
         (setq i (1+ i))
         (let ((s (raindrop-search--format-candidate it)))
           (puthash s it raindrop-search--cand-map)
           (propertize s 'raindrop-id (raindrop-search--item-id it)
                       'raindrop-index i)))
       raindrop-search--items)))
   (t '())))

(defun raindrop-search--item-id (it)
  "Extract ID from IT."
  (or (raindrop-search--kv it 'id)
      (raindrop-search--kv it :id)
      (raindrop-search--kv it '_id)
      (raindrop-search--kv it :_id)))

(defun raindrop-search--default-raindrop-url-builder (item)
  "Build default Raindrop app URL for ITEM."
  (let* ((cid (or (raindrop-search--kv item :collectionId)
                  (raindrop-search--kv item 'collectionId)))
         (id  (raindrop-search--item-id item)))
    (cond
     ((eq cid -1) (format "https://app.raindrop.io/#/unsorted/%d" id))
     ((and (integerp cid) (integerp id))
      (format "https://app.raindrop.io/#/collection/%d/%d" cid id))
     ((integerp cid)
      (format "https://app.raindrop.io/#/collection/%d" cid))
     (t "https://app.raindrop.io/"))))

(defun raindrop-search--open-item (item)
  "Open ITEM based on configured action."
  (pcase raindrop-search-enter-action
    ('raindrop (and-let* ((url (funcall raindrop-search-raindrop-url-builder item)))
                 (browse-url url)))
    (_ (and-let* ((url (or (raindrop-search--kv item :link)
                           (raindrop-search--kv item 'link))))
         (browse-url url)))))

(defun raindrop-search--exit (cand)
  "Exit with selected CAND."
  (let* ((idx (cl-position cand (raindrop-search--ui-candidates) :test #'string=))
         (it (or (and idx (nth idx raindrop-search--items))
                 (and (stringp cand) (gethash cand raindrop-search--cand-map)))))
    (when it (raindrop-search--open-item it))))

;;;###autoload
(defun raindrop-search-toggle-enter-action ()
  "Toggle what pressing RET does in `raindrop-search'.
Switches between opening the original link and opening the Raindrop
app URL for the item."
  (interactive)
  (setq raindrop-search-enter-action (pcase raindrop-search-enter-action
                                       ('link 'raindrop) (_ 'link)))
  (raindrop-search--debug "Enter action → %s" raindrop-search-enter-action))

(defun raindrop-search--minibuffer-setup ()
  "Setup minibuffer for raindrop search."
  (when (minibufferp)
    (add-hook 'post-command-hook #'raindrop-search--schedule nil t)
    (raindrop-search--schedule)
    (raindrop-search--ensure-collections)))

(defun raindrop-search--minibuffer-cleanup ()
  "Cleanup minibuffer after raindrop search."
  (when (minibufferp)
    (remove-hook 'post-command-hook #'raindrop-search--schedule t))
  (when raindrop-search--timer
    (cancel-timer raindrop-search--timer)
    (setq raindrop-search--timer nil))
  (raindrop-search--stop-spinner))

;;;###autoload
(defun raindrop-search ()
  "Start interactive Raindrop search with as-you-type completion."
  (interactive)
  (setq raindrop-search--items nil
        raindrop-search--last-gen 0)
  (minibuffer-with-setup-hook #'raindrop-search--minibuffer-setup
    (unwind-protect
        (let ((table (lambda (_str _pred action)
                       (pcase action
                         ('metadata '(metadata (category . raindrop)))
                         (_ (raindrop-search--ui-candidates))))))
          (and-let* ((res (completing-read "Raindrop: " table nil t)))
            (unless (string-empty-p res)
              (raindrop-search--exit res))))
      (raindrop-search--minibuffer-cleanup))))

(defvar raindrop-search-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'raindrop-search-edit-save)
    (define-key m (kbd "C-c C-k") #'raindrop-search-edit-cancel)
    m)
  "Keymap for Raindrop edit mode.")

(define-derived-mode raindrop-search-edit-mode text-mode "Raindrop-Edit"
  "Major mode for editing a Raindrop item."
  (setq buffer-read-only nil)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults nil))

(defun raindrop-search--edit-buffer-name (id)
  "Generate buffer name for editing item with ID."
  (format "*Raindrop Edit %s*" id))

(defun raindrop-search--insert-ro-key (label)
  "Insert LABEL as read-only with face."
  (let ((start (point)))
    (insert label)
    (add-text-properties
     start (point)
     `(read-only t
                 front-sticky t
                 rear-nonsticky t
                 face raindrop-search-edit-key
                 font-lock-face raindrop-search-edit-key))))

(defun raindrop-search--open-edit-buffer (item)
  "Open edit buffer for ITEM."
  (let* ((id (raindrop-search--item-id item))
         (buf (get-buffer-create (raindrop-search--edit-buffer-name (or id "unknown"))))
         (title (substring-no-properties (or (raindrop-search--kv item :title)
                                             (raindrop-search--kv item 'title) "")))
         (link  (substring-no-properties (or (raindrop-search--kv item :link)
                                             (raindrop-search--kv item 'link) "")))
         (tags  (raindrop-search--tags->strings
                 (or (raindrop-search--kv item :tags)
                     (raindrop-search--kv item 'tags))))
         (excerpt (substring-no-properties (or (raindrop-search--kv item :excerpt)
                                               (raindrop-search--kv item 'excerpt) ""))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "# Edit Raindrop item. Press C-c C-c to save, C-c C-k to cancel."
                            'face 'raindrop-search-edit-header))
        (insert "\n\n")
        (raindrop-search--insert-ro-key "ID: ")
        (insert (format "%s\n" (or id "unknown")))
        (raindrop-search--insert-ro-key "Title: ")
        (insert (format "%s\n" title))
        (raindrop-search--insert-ro-key "Link: ")
        (insert (format "%s\n" link))
        (raindrop-search--insert-ro-key "Tags: ")
        (insert (mapconcat (lambda (tag)
                             (if (string-match-p "[ ,\"]" tag)
                                 (format "\"%s\"" tag) tag))
                           tags ", "))
        (insert "\n")
        (raindrop-search--insert-ro-key "Excerpt:")
        (insert "\n")
        (insert excerpt))
      (goto-char (point-min))
      (raindrop-search-edit-mode)
      (setq raindrop-search--edit-item-id id))
    (pop-to-buffer buf)
    (and-let* ((win (get-buffer-window buf)))
      (select-window win))
    (goto-char (point-min))
    (re-search-forward "^Title:\\s-*" nil t)
    (when (and (active-minibuffer-window)
               (minibufferp (window-buffer (active-minibuffer-window))))
      (run-at-time 0 nil (lambda ()
                           (when (and (minibufferp)
                                      (fboundp 'abort-recursive-edit))
                             (abort-recursive-edit)))))
    buf))

(defun raindrop-search--parse-edit-buffer ()
  "Parse current edit buffer into update payload."
  (save-excursion
    (goto-char (point-min))
    (let (title link tags excerpt)
      (when (re-search-forward "^Title:\\s-*\\(.*\\)$" nil t)
        (setq title (string-trim (substring-no-properties (match-string 1)))))
      (goto-char (point-min))
      (when (re-search-forward "^Link:\\s-*\\(.*\\)$" nil t)
        (setq link (string-trim (substring-no-properties (match-string 1)))))
      (goto-char (point-min))
      (when (re-search-forward "^Tags:\\s-*\\(.*\\)$" nil t)
        (let* ((raw (substring-no-properties (match-string 1)))
               (parts (split-string raw "\\s-*,\\s-*" t))
               (norm (mapcar (lambda (s)
                               (setq s (string-trim s))
                               (if (and (>= (length s) 2)
                                        (string-prefix-p "\"" s)
                                        (string-suffix-p "\"" s))
                                   (substring s 1 (1- (length s)))
                                 s))
                             parts)))
          (setq tags norm)))
      (goto-char (point-min))
      (when (re-search-forward "^Excerpt:\\s-*$" nil t)
        (setq excerpt (string-trim (buffer-substring-no-properties (point) (point-max)))))
      (seq-filter (lambda (kv)
                    (let ((val (cdr kv)))
                      (and val (if (listp val) (not (null val)) (not (string-empty-p val))))))
                  `((title . ,title)
                    (link . ,link)
                    (tags . ,tags)
                    (excerpt . ,excerpt))))))

;;;###autoload
(defun raindrop-search-embark-open-link (cand)
  "Open the original link for CAND."
  (let* ((idx (cl-position cand (raindrop-search--ui-candidates) :test #'string=))
         (it (or (and idx (nth idx raindrop-search--items))
                 (and (stringp cand) (gethash cand raindrop-search--cand-map)))))
    (if it
        (and-let* ((url (or (raindrop-search--kv it :link)
                            (raindrop-search--kv it 'link))))
          (browse-url url))
      (message "No item for this candidate."))))

;;;###autoload
(defun raindrop-search-embark-open-raindrop (cand)
  "Open the Raindrop app URL for CAND."
  (let* ((idx (cl-position cand (raindrop-search--ui-candidates) :test #'string=))
         (it (or (and idx (nth idx raindrop-search--items))
                 (and (stringp cand) (gethash cand raindrop-search--cand-map)))))
    (if it
        (and-let* ((url (funcall raindrop-search-raindrop-url-builder it)))
          (browse-url url))
      (message "No item for this candidate."))))

;;;###autoload
(defun raindrop-search-embark-edit (cand)
  "Open an editable buffer for the Raindrop item represented by CAND."
  (let* ((idx (cl-position cand (raindrop-search--ui-candidates) :test #'string=))
         (it (or (and idx (nth idx raindrop-search--items))
                 (and (stringp cand) (gethash cand raindrop-search--cand-map)))))
    (if it (raindrop-search--open-edit-buffer it)
      (message "No item for this candidate."))))

;;;###autoload
(defun raindrop-search-embark-delete (cand)
  "Delete the Raindrop item represented by CAND after confirmation."
  (let* ((idx (cl-position cand (raindrop-search--ui-candidates) :test #'string=))
         (it (or (and idx (nth idx raindrop-search--items))
                 (and (stringp cand) (gethash cand raindrop-search--cand-map)))))
    (if (not it)
        (message "No item for this candidate.")
      (when (y-or-n-p "Delete this Raindrop? ")
        (raindrop-api-request-async
         (format "/raindrop/%d" (raindrop-search--item-id it))
         'DELETE nil nil
         (lambda (_res err)
           (if err (message "Delete failed: %s" err)
             (message "Deleted"))))))))

;;;###autoload
(defun raindrop-search-edit-save ()
  "Save changes from the current Raindrop edit buffer back to Raindrop."
  (interactive)
  (let* ((id raindrop-search--edit-item-id)
         (payload (raindrop-search--parse-edit-buffer))
         (is-creation (null id)))
    (if is-creation
        ;; Create new bookmark
        (progn
          ;; Ensure link is present for creation
          (unless (alist-get 'link payload)
            (user-error "Link is required for creating a bookmark"))
          (raindrop-api-request-async
           "/raindrop" 'POST nil payload
           (lambda (res err)
             (if err 
                 (message "Create failed: %s" err)
               (message "Bookmark created successfully")
               (quit-window t (selected-window))))))
      ;; Update existing bookmark
      (if (not (integerp id))
          (user-error "No valid item ID available for saving")
        (raindrop-api-request-async
         (format "/raindrop/%d" id) 'PUT nil payload
         (lambda (_res err)
           (if err (message "Save failed: %s" err)
             (message "Saved")
             (quit-window t (selected-window)))))))))

;;;###autoload
(defun raindrop-search-edit-cancel ()
  "Abort editing the Raindrop item and close the edit buffer."
  (interactive)
  (quit-window t (selected-window)))

;;;###autoload
(defun raindrop-search-create-bookmark (url &optional title collection)
  "Create a new bookmark with URL.
Optionally specify TITLE and COLLECTION.
Opens edit buffer for further customization."
  (interactive 
   (let ((url (read-string "URL: "
                          (or (thing-at-point 'url)
                              (when (fboundp 'browse-url-url-at-point)
                                (browse-url-url-at-point))
                              ""))))
     (list url)))
  (unless (string-match-p "^https?://" url)
    (user-error "Invalid URL: %s" url))
  (let ((new-item `((title . ,(or title ""))
                    (link . ,url)
                    (tags . ())
                    (excerpt . "")
                    (collection . ,(when collection `((id . ,collection)))))))
    (raindrop-search--open-create-buffer new-item)))

(defun raindrop-search--open-create-buffer (item)
  "Open create buffer for new ITEM."
  (let* ((buf (get-buffer-create "*Raindrop Create*"))
         (title (or (raindrop-search--kv item :title)
                    (raindrop-search--kv item 'title) ""))
         (link  (or (raindrop-search--kv item :link)
                    (raindrop-search--kv item 'link) ""))
         (tags  (raindrop-search--tags->strings
                 (or (raindrop-search--kv item :tags)
                     (raindrop-search--kv item 'tags))))
         (excerpt (or (raindrop-search--kv item :excerpt)
                      (raindrop-search--kv item 'excerpt) "")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize "# Create new Raindrop bookmark. Press C-c C-c to save, C-c C-k to cancel."
                            'face 'raindrop-search-edit-header))
        (insert "\n\n")
        (raindrop-search--insert-ro-key "Title: ")
        (insert (format "%s\n" title))
        (raindrop-search--insert-ro-key "Link: ")
        (insert (format "%s\n" link))
        (raindrop-search--insert-ro-key "Tags: ")
        (insert (mapconcat (lambda (tag)
                             (if (string-match-p "[ ,\"]" tag)
                                 (format "\"%s\"" tag) tag))
                           tags ", "))
        (insert "\n")
        (raindrop-search--insert-ro-key "Excerpt:")
        (insert "\n")
        (insert excerpt))
      (goto-char (point-min))
      (raindrop-search-edit-mode)
      (setq raindrop-search--edit-item-id nil)) ; nil indicates creation mode
    (pop-to-buffer buf)
    (and-let* ((win (get-buffer-window buf)))
      (select-window win))
    (goto-char (point-min))
    (re-search-forward "^Title:\\s-*" nil t)
    (when (and (active-minibuffer-window)
               (minibufferp (window-buffer (active-minibuffer-window))))
      (run-at-time 0 nil (lambda ()
                           (when (and (minibufferp)
                                      (fboundp 'abort-recursive-edit))
                             (abort-recursive-edit)))))
    buf))

;;;###autoload
(defun raindrop-search-create-from-browser ()
  "Create a bookmark from the current browser tab (requires browser extension or external tool)."
  (interactive)
  (let ((url (read-string "URL (from clipboard or browser): " 
                          (or (current-kill 0 t)
                              ""))))
    (raindrop-search-create-bookmark url)))

;;;###autoload  
(defun raindrop-search-create-from-kill-ring ()
  "Create a bookmark from URL in kill ring."
  (interactive)
  (let ((url (current-kill 0 t)))
    (when (and url (string-match-p "^https?://" url))
      (raindrop-search-create-bookmark url))))

(with-eval-after-load 'embark
  (defvar embark-keymap-alist)
  (defvar raindrop-search-embark-map
    (let ((m (make-sparse-keymap)))
      (define-key m (kbd "o") #'raindrop-search-embark-open-link)
      (define-key m (kbd "O") #'raindrop-search-embark-open-raindrop)
      (define-key m (kbd "e") #'raindrop-search-embark-edit)
      (define-key m (kbd "D") #'raindrop-search-embark-delete)
      (define-key m (kbd "c") #'raindrop-search-create-from-browser)
      (define-key m (kbd "C") #'raindrop-search-create-from-kill-ring)
      m)
    "Embark keymap for `raindrop' completion category.")
  (add-to-list 'embark-keymap-alist '(raindrop . raindrop-search-embark-map)))

(provide 'raindrop-search)
;;; raindrop-search.el ends here
