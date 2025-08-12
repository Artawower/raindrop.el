;;; raindrop-search.el --- As-you-type Raindrop search completion  -*- lexical-binding: t; -*-

;; Author: you
;; Version: 0.9
;; Package-Requires: ((emacs "27.1") (raindrop "0.1") (request "0.3.0") (json "1.4"))
;; Keywords: convenience, matching
;; URL: https://github.com/your/repo

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'cl-lib)
(require 'json)
(require 'request)
(require 'raindrop)

;; ---------- Faces ----------
(defface raindrop-search-tag
  '((t :inherit font-lock-keyword-face :foreground "DodgerBlue3"))
  "Face for tags in completion results.")

(defface raindrop-search-collection
  '((t :inherit font-lock-function-name-face :foreground "orange"))
  "Face for collections in completion results.")

(defface raindrop-search-edit-key
  '((t :inherit font-lock-keyword-face))
  "Face used for read-only field labels in the edit buffer.")

(defface raindrop-search-edit-header
  '((t :inherit shadow :weight normal))
  "Face used for the header line in the edit buffer.")

;; ---------- Customization ----------
(defgroup raindrop-search nil
  "Completion UI for Raindrop."
  :group 'convenience)

(defcustom raindrop-search-title-max 20
  "Maximum number of characters shown for titles in completion candidates
before truncation."
  :type 'integer)
(defcustom raindrop-search-excerpt-max 40
  "Maximum number of characters shown for the excerpt/description part
of a completion candidate."
  :type 'integer)
(defcustom raindrop-search-idle-delay 0.25
  "Idle time in seconds before firing a Raindrop API request after the
minibuffer input changes."
  :type 'number)
(defcustom raindrop-search-page-size 50
  "Number of items to request per page from the Raindrop API (1..50)."
  :type 'integer)
(defcustom raindrop-search-text-collection 0
  "Default collection ID to search when no [collection] token is used.
0 means the root (\"All\"), while -1 refers to \"Unsorted\"."
  :type 'integer :group 'raindrop)
(defcustom raindrop-search-enter-action 'link
  "Enter opens either original link or Raindrop app."
  :type '(choice (const link) (const raindrop)))
(defcustom raindrop-search-raindrop-url-builder
  #'raindrop-search--default-raindrop-url-builder
  "Function used to build a Raindrop app URL for ITEM.
Called with a normalized Raindrop item plist/alist and must return a
URL string."
  :type 'function)

;; ---------- State ----------
(defvar raindrop-search--timer nil)
(defvar raindrop-search--last-gen 0)
(defvar raindrop-search--items nil)
(defvar raindrop-search--collections-loading nil)
(defvar raindrop-search--collections-ready nil)
(defvar raindrop-search--collections-by-title nil)
(defvar raindrop-search--collections-by-id nil)
(defvar raindrop-search--last-input nil)
(defvar raindrop-search--cand-map (make-hash-table :test 'equal))

;; ---------- Utils ----------
(defun raindrop-search--log (fmt &rest args)
  (message "[raindrop-search] %s" (apply #'format fmt args)))

(defun raindrop-search--truncate (s n)
  (if (and (stringp s) (> (length s) n))
      (concat (substring s 0 n) "…")
    (or s "")))

(defun raindrop-search--kv (it key)
  (let* ((k1 key)
         (k2 (if (keywordp key) (intern (substring (symbol-name key) 1))
               (intern (format ":%s" key)))))
    (or (plist-get it k1)
        (plist-get it k2)
        (and (consp it) (alist-get k1 it))
        (and (consp it) (alist-get k2 it)))))

(defun raindrop-search--safe-exhibit ()
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
              (when (fboundp 'vertico--update)
                (vertico--update))
              (when (fboundp 'vertico--exhibit)
                (vertico--exhibit))))
           ((and (boundp 'ivy-mode) ivy-mode (fboundp 'ivy--exhibit))
            (ivy--exhibit))
           ((and (boundp 'icomplete-mode) icomplete-mode (fboundp 'icomplete-exhibit))
            (icomplete-exhibit))
           ((fboundp 'mct--exhibit) 
            (mct--exhibit))
           ((and (boundp 'selectrum-mode) selectrum-mode (fboundp 'selectrum--update))
            (selectrum--update))))))))

;; ---------- Collections cache (fix: correct hash test; always refresh UI after load) ----------
(defun raindrop-search--collections-build-index (items)
  (let ((by-title (make-hash-table :test 'equal))  ;; titles are lowercase strings
        (by-id    (make-hash-table :test 'eql)))
    (dolist (c items)
      (let* ((id (or (raindrop-search--kv c '_id)
                     (raindrop-search--kv c :_id)
                     (raindrop-search--kv c 'id)
                     (raindrop-search--kv c :id)))
             (title (or (raindrop-search--kv c 'title)
                        (raindrop-search--kv c :title)
                        (raindrop-search--kv c 'name)
                        (raindrop-search--kv c :name))))
        (when (and id title)
          (puthash (downcase title) id by-title)
          (puthash id title by-id))))
    (setq raindrop-search--collections-by-title by-title
          raindrop-search--collections-by-id by-id)))

(defun raindrop-search--ensure-collections ()
  (unless (or raindrop-search--collections-ready
              raindrop-search--collections-loading)
    (setq raindrop-search--collections-loading t)
    (raindrop-search--log "loading collections…")
    (raindrop-api-request-async
     "/collections" 'GET nil nil
     (lambda (res err)
       (setq raindrop-search--collections-loading nil)
       (if err
           (raindrop-search--log "collections load error: %s" err)
         (let ((items (alist-get 'items res)))
           ;; Конвертируем вектор в список, если необходимо
           (when (vectorp items)
             (setq items (append items nil)))
           (raindrop-search--collections-build-index (or items '()))))
       (setq raindrop-search--collections-ready t)
       ;; important: repaint candidates once titles are available
       (raindrop-search--safe-exhibit)))))

(defun raindrop-search--collection-id-by-title (name)
  (when (and name raindrop-search--collections-by-title)
    (gethash (downcase name) raindrop-search--collections-by-title)))

(defun raindrop-search--collection-title-by-id (cid)
  (cond
   ((eq cid -1) "Unsorted")
   ((hash-table-p raindrop-search--collections-by-id)
    (gethash cid raindrop-search--collections-by-id))
   (t nil)))

;; ---------- Parsing / search building ----------
(defun raindrop-search--parse (s)
  (let* ((s (or s ""))
         (tokens (split-string s "[ \t]+" t))
         (tags '())
         (folders '())
         (texts '())
         (tag-re (rx string-start "#" (group (+ (any alnum ?- ?_ ?+ ?.))) string-end))
         (folder-re (rx string-start "[" (group (*? (not (any "]")))) "]" string-end)))
    (dolist (tok tokens)
      (cond
       ((string-match tag-re tok) (push (downcase (match-string 1 tok)) tags))
       ((string-match folder-re tok) (push (match-string 1 tok) folders))
       (t (push tok texts))))
    (list :tags (nreverse tags)
          :folders (nreverse folders)
          :text (string-join (nreverse texts) " "))))

(defun raindrop-search--meaningful-input-p (parsed)
  (let ((text (plist-get parsed :text))
        (tags (plist-get parsed :tags))
        (folders (plist-get parsed :folders)))
    (or (and text (>= (length text) 2))
        (and (listp tags) (> (length tags) 0))
        (and (listp folders) (> (length folders) 0)))))

(defun raindrop-search--compose-search (text tags)
  (string-join
   (delq nil
         (list (and (listp tags) tags (raindrop--build-tag-search tags 'all))
               (and text (not (string-empty-p text)) text)))
   " "))

(defun raindrop-search--endpoint-and-query (collection-id search page)
  (let* ((cid (if (numberp collection-id) collection-id raindrop-search-text-collection))
         (pp  (max 1 (min raindrop-search-page-size 50)))
         (pg  (max 0 (or page 0)))
         (endpoint (format "/raindrops/%d" cid))
         (q (seq-filter #'consp
                        (append
                         (when (and search (not (string-empty-p search)))
                           (list (cons 'search search)))
                         (list (cons 'perpage pp)
                               (cons 'page pg)
                               (cons 'expand "collection"))))))
    (cons endpoint q)))

(defun raindrop-search--domain-of (link item)
  (or (raindrop-search--kv item 'domain)
      (and (stringp link)
           (when (string-match "//\\([^/]+\\)" link)
             (match-string 1 link)))))

(defun raindrop-search--tags->strings (tags)
  (cond
   ((null tags) nil)
   ((vectorp tags) (mapcar (lambda (x) (downcase (format "%s" x))) (append tags nil)))
   ((listp tags)   (mapcar (lambda (x) (downcase (format "%s" x))) tags))
   (t nil)))

;; ---------- Formatting (ensure collection title) ----------
(defun raindrop-search--format-candidate (it)
  (let* ((title   (or (raindrop-search--kv it :title)
                      (raindrop-search--kv it 'title) ""))
         (excerpt (or (raindrop-search--kv it :excerpt)
                      (raindrop-search--kv it 'excerpt) ""))
         (desc*   (raindrop-search--truncate (string-trim (if (string-empty-p excerpt) title excerpt))
                                             raindrop-search-excerpt-max))
         (link    (or (raindrop-search--kv it :link)
                      (raindrop-search--kv it 'link) ""))
         (domain* (when-let ((d (raindrop-search--domain-of link it)))
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
    (string-join (delq nil (list collstr desc* tagstr domain*)) "  ")))

(defun raindrop-search--apply-results (gen items)
  (when (= gen raindrop-search--last-gen)
    (setq raindrop-search--items items)
    (raindrop-search--safe-exhibit)))

;; ---------- Networking ----------
(defun raindrop-api-request-async (endpoint method params body callback)
  (let* ((url (concat raindrop-api-base endpoint))
         (token (raindrop--get-token-from-auth-source))
         (headers `(("Authorization" . ,(concat "Bearer " token))
                    ("Content-Type" . "application/json")))
         (data (when body (json-encode body)))
         (type (pcase method ('GET "GET") ('PUT "PUT") ('POST "POST") ('DELETE "DELETE") (_ "GET"))))
    (request url
      :type type :headers headers :params params :data data
      :parser 'json-read :encoding 'utf-8
      :success (cl-function (lambda (&key data &allow-other-keys)
                              (funcall callback data nil)))
      :error   (cl-function (lambda (&key error-thrown &allow-other-keys)
                              (funcall callback nil (format "%s" error-thrown)))))))

(defun raindrop-search--fetch (gen page parsed)
  (let* ((tags    (plist-get parsed :tags))
         (folders (plist-get parsed :folders))
         (text    (plist-get parsed :text))
         (folder-name (car (last folders)))
         (coll-id (or (and folder-name
                           (raindrop-search--collection-id-by-title folder-name))
                      raindrop-search-text-collection))
         (search (raindrop-search--compose-search text tags))
         (pair   (raindrop-search--endpoint-and-query coll-id search page))
         (endpoint (car pair))
         (query    (cdr pair)))
    (raindrop-search--log "fetch(gen=%s) search=%S endpoint=%s" gen (if (string-empty-p search) nil search) endpoint)
    (raindrop-api-request-async
     endpoint 'GET query nil
     (lambda (res err)
       (when (and err (string-match-p "HTTP 404\\|Network error" err))
         (setq res '((items))))
       (when (= gen raindrop-search--last-gen)
         (if err
             (progn (message "Raindrop error: %s" err)
                    (raindrop-search--apply-results gen nil))
           (let* ((raw  (or (alist-get 'items res) '()))
                  (norm (mapcar (lambda (x) (condition-case nil
                                                (raindrop--normalize-item x)
                                              (error x)))
                                raw)))
             (raindrop-search--apply-results gen norm))))))))

;; ---------- UI flow ----------
(defun raindrop-search--idle-fire ()
  (let* ((input (minibuffer-contents-no-properties))
         (parsed (raindrop-search--parse input)))
    (raindrop-search--log "idle fire input=%S" input)
    (when (raindrop-search--meaningful-input-p parsed)
      (setq raindrop-search--last-gen (1+ raindrop-search--last-gen))
      (raindrop-search--fetch raindrop-search--last-gen 0 parsed))))

(defun raindrop-search--schedule ()
  (let ((input (minibuffer-contents-no-properties)))
    (unless (equal input raindrop-search--last-input)
      (setq raindrop-search--last-input input)
      (setq raindrop-search--items nil)
      (raindrop-search--safe-exhibit)
      (setq raindrop-search--last-gen (1+ raindrop-search--last-gen))))
  (when raindrop-search--timer (cancel-timer raindrop-search--timer))
  (setq raindrop-search--timer
        (run-with-idle-timer raindrop-search-idle-delay nil
                             #'raindrop-search--idle-fire)))

(defun raindrop-search--ui-candidates ()
  (setq raindrop-search--cand-map (make-hash-table :test 'equal))
  (let ((i -1))
    (mapcar
     (lambda (it)
       (setq i (1+ i))
       (let ((s (raindrop-search--format-candidate it)))
         (puthash s it raindrop-search--cand-map)
         (propertize s 'raindrop-id (raindrop-search--item-id it)
                     'raindrop-index i)))
     (or raindrop-search--items '()))))

(defun raindrop-search--item-id (it)
  (or (raindrop-search--kv it 'id)
      (raindrop-search--kv it :id)
      (raindrop-search--kv it '_id)
      (raindrop-search--kv it :_id)))

(defun raindrop-search--default-raindrop-url-builder (item)
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
  (pcase raindrop-search-enter-action
    ('raindrop (when-let ((url (funcall raindrop-search-raindrop-url-builder item)))
                 (browse-url url)))
    (_ (when-let ((url (or (raindrop-search--kv item :link)
                           (raindrop-search--kv item 'link))))
         (browse-url url)))))

(defun raindrop-search--exit (cand)
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
  (message "raindrop-search: Enter action → %s" raindrop-search-enter-action))

(defun raindrop-search--minibuffer-setup ()
  (when (minibufferp)
    (add-hook 'post-command-hook #'raindrop-search--schedule nil t)
    (raindrop-search--schedule)
    (raindrop-search--ensure-collections)))

(defun raindrop-search--minibuffer-cleanup ()
  (when (minibufferp)
    (remove-hook 'post-command-hook #'raindrop-search--schedule t))
  (when raindrop-search--timer
    (cancel-timer raindrop-search--timer)
    (setq raindrop-search--timer nil)))

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
          (when-let ((res (completing-read "Raindrop: " table nil t)))
            (unless (string-empty-p res)
              (raindrop-search--exit res))))
      (raindrop-search--minibuffer-cleanup))))

;; ---------- Embark integration: Edit buffer with colored, read-only keys ----------
(defvar raindrop-search-edit-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'raindrop-search-edit-save)
    (define-key m (kbd "C-c C-k") #'raindrop-search-edit-cancel)
    m))

(define-derived-mode raindrop-search-edit-mode text-mode "Raindrop-Edit"
  "Major mode for editing a Raindrop item."
  (setq buffer-read-only nil)
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  ;; don't fight our explicit faces
  (setq-local font-lock-defaults nil))

(defun raindrop-search--edit-buffer-name (id)
  (format "*Raindrop Edit %s*" id))

(defun raindrop-search--insert-ro-key (label)
  "Insert LABEL (like \"Title:\") as read-only with face."
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
        ;; header
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
      (setq-local raindrop-search--edit-item-id id))
    ;; Display and focus the edit buffer *now*; if invoked from a minibuffer
    ;; (e.g., via Embark), also quit the minibuffer after switching focus.
    (pop-to-buffer buf)
    (when-let ((win (get-buffer-window buf)))
      (select-window win))
    (goto-char (point-min))
    (re-search-forward "^Title:\\s-*" nil t)
    ;; If we're currently inside a live minibuffer, close it asynchronously
    ;; so point/focus truly land in the edit buffer.
    (when (and (active-minibuffer-window)
               (minibufferp (window-buffer (active-minibuffer-window))))
      (run-at-time 0 nil (lambda () (when (minibufferp)
                                      (minibuffer-keyboard-quit)))))
    buf))

(defun raindrop-search--parse-edit-buffer ()
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
(defun raindrop-search-embark-edit (cand)
  "Open an editable buffer for the Raindrop item represented by CAND.
CAND is a completion string from the minibuffer list."
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
  (unless (boundp 'raindrop-search--edit-item-id)
    (user-error "Not a raindrop edit buffer"))
  (let ((id raindrop-search--edit-item-id))
    (if (or (null id) (not (integerp id)))
        (user-error "No valid item ID available for saving")
      (let* ((payload (raindrop-search--parse-edit-buffer)))
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

(with-eval-after-load 'embark
  (defvar raindrop-search-embark-map
    (let ((m (make-sparse-keymap)))
      (define-key m (kbd "o") #'raindrop-search-embark-open-link)
      (define-key m (kbd "O") #'raindrop-search-embark-open-raindrop)
      (define-key m (kbd "e") #'raindrop-search-embark-edit)
      (define-key m (kbd "D") #'raindrop-search-embark-delete)
      m)
    "Embark keymap for `raindrop' completion category.")
  (add-to-list 'embark-keymap-alist '(raindrop . raindrop-search-embark-map)))

(provide 'raindrop-search)
;;; raindrop-search.el ends here
