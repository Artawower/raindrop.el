;;; raindrop-search.el --- As-you-type Raindrop search completion  -*- lexical-binding: t; -*-

;; Author: you
;; Version: 0.6
;; Package-Requires: ((emacs "27.1") (raindrop "0.1"))
;; Keywords: convenience, matching
;; URL: https://github.com/your/repo

;;; Commentary:
;; Incremental, asynchronous Raindrop search in the minibuffer with tags (#tag)
;; and collection selector ([Group]). Collections are preloaded and cached; a
;; matching [Group] narrows the query to /raindrops/<id>, otherwise /raindrops/0.
;; Tags and text are composed into the search string; the [Group] token is used
;; only to choose collectionId and is not injected into the search string.
;; Pagination starts at page 0. Results are rendered as:
;;
;;   [Collection]  short description  #tags  domain
;;
;; The code tolerates both alists and plists coming from the API, and both
;; keyword (:title) and symbol ('title) keys. The UI refreshes automatically
;; after async responses, without requiring cursor motion. The refresh function
;; attempts to support Vertico, Icomplete, Ivy, Selectrum and MCT; it falls back
;; to a small input nudge if no known frontend is active.
;;
;; Enter behavior is configurable via `raindrop-search-enter-action':
;;   - 'link     → open the original bookmark link
;;   - 'raindrop → open the bookmark inside Raindrop app (URL is configurable)

;;; Code:

(require 'subr-x)
(require 'seq)
(require 'cl-lib)
(require 'raindrop)

(eval-when-compile
  (declare-function vertico--exhibit "vertico")
  (declare-function ivy--exhibit "ivy")
  (declare-function icomplete-exhibit "icomplete")
  (declare-function selectrum--update "selectrum")
  (declare-function mct--exhibit "mct"))

(defgroup raindrop-search nil
  "Completion UI for Raindrop."
  :group 'convenience)

(defface raindrop-search-tag
  '((t :inherit font-lock-keyword-face :foreground "DodgerBlue3"))
  "Face for tags in completion results.")

(defface raindrop-search-collection
  '((t :inherit font-lock-function-name-face :foreground "orange"))
  "Face for collections in completion results.")

(defcustom raindrop-search-title-max 20
  "Visible width for titles before truncation."
  :type 'integer)

(defcustom raindrop-search-excerpt-max 40
  "Visible width for excerpts before truncation."
  :type 'integer)

(defcustom raindrop-search-idle-delay 0.25
  "Idle delay before firing the asynchronous search."
  :type 'number)

(defcustom raindrop-search-page-size 50
  "Page size for Raindrop pagination."
  :type 'integer)

(defcustom raindrop-search-text-collection 0
  "Collection id used when no collection is selected."
  :type 'integer :group 'raindrop)

(defcustom raindrop-search-enter-action 'link
  "What pressing Enter should do in `raindrop-search'.
- 'link: open the bookmark's original URL.
- 'raindrop: open the bookmark in the Raindrop app."
  :type '(choice (const :tag "Open original link" link)
                 (const :tag "Open in Raindrop app" raindrop))
  :group 'raindrop-search)

(defcustom raindrop-search-raindrop-url-builder #'raindrop-search--default-raindrop-url-builder
  "Function that builds a Raindrop app URL for an ITEM.
Called with one argument: the normalized Raindrop ITEM (alist or plist).
Should return a string URL or nil. If nil is returned, a fallback to
collection-only URL is attempted, and then to the app root."
  :type 'function
  :group 'raindrop-search)

(defvar raindrop-search--timer nil)
(defvar raindrop-search--last-gen 0)
(defvar raindrop-search--items nil)
(defvar raindrop-search--collections-loading nil)
(defvar raindrop-search--collections-ready nil)
(defvar raindrop-search--collections-by-title nil)
(defvar raindrop-search--collections-by-id nil)
(defvar raindrop-search--last-input nil)

(defun raindrop-search--log (fmt &rest args)
  "Log a debug line with FMT and ARGS."
  (message "[raindrop-search] %s" (apply #'format fmt args)))

(defun raindrop-search--truncate (s n)
  "Return S truncated to N chars with ellipsis."
  (if (and (stringp s) (> (length s) n))
      (concat (substring s 0 n) "…")
    (or s "")))

(defun raindrop-search--safe-exhibit ()
  "Refresh the active minibuffer completion UI after async results arrive.
Tries to call the active frontend's own exhibit function; falls back to
a tiny input nudge to trigger a redisplay if no known frontend is active."
  (let ((win (active-minibuffer-window))
        (gen raindrop-search--last-gen))
    (when (and win (minibufferp (window-buffer win)))
      (run-at-time
       0 nil
       (lambda ()
         (when (and (eq win (active-minibuffer-window))
                    (= gen raindrop-search--last-gen))
           (with-current-buffer (window-buffer win))
           (cond
            ((and (boundp 'vertico-mode) vertico-mode
                  (fboundp 'vertico--exhibit))
             (when (boundp 'vertico--input) (setq vertico--input nil))
             (when (boundp 'vertico--dirty) (setq-local vertico--dirty t))
             (ignore-errors (vertico--exhibit)))
            ((and (boundp 'ivy-mode) ivy-mode
                  (fboundp 'ivy--exhibit))
             (ignore-errors (ivy--exhibit)))
            ((and (boundp 'icomplete-mode) icomplete-mode
                  (fboundp 'icomplete-exhibit))
             (ignore-errors (icomplete-exhibit)))
            ((fboundp 'mct--exhibit)
             (ignore-errors (mct--exhibit)))
            ((and (boundp 'selectrum-mode) selectrum-mode
                  (fboundp 'selectrum--update))
             (ignore-errors (selectrum--update)))
            (t
             (let ((pt (point)))
               (insert " ") (delete-char -1) (goto-char pt)))))
         (redisplay))))))

(defun raindrop-search--kv (it key)
  "Return value for KEY from IT, accepting both alists and plists and both keyword and symbol keys."
  (let* ((k1 key)
         (k2 (if (keywordp key) (intern (substring (symbol-name key) 1)) (intern (format ":%s" key)))))
    (or (plist-get it k1)
        (plist-get it k2)
        (and (consp it) (alist-get k1 it))
        (and (consp it) (alist-get k2 it)))))

(defun raindrop-search--kv-in (it &rest path)
  "Return nested value following PATH from IT."
  (let ((cur it))
    (dolist (k path)
      (setq cur (cond
                 ((null cur) nil)
                 ((hash-table-p cur) (gethash k cur))
                 (t (raindrop-search--kv cur k)))))
    cur))

(defun raindrop-search--collections-build-index (items)
  "Build collection indices from ITEMS."
  (let ((by-title (make-hash-table :test 'equal))
        (by-id (make-hash-table :test 'eql)))
    (dolist (c items)
      (let* ((id (or (raindrop-search--kv c '_id)
                     (raindrop-search--kv c :_id)))
             (title (or (raindrop-search--kv c 'title)
                        (raindrop-search--kv c :title))))
        (when (and id title)
          (puthash (downcase title) id by-title)
          (puthash id title by-id))))
    (setq raindrop-search--collections-by-title by-title
          raindrop-search--collections-by-id by-id)))

(defun raindrop-search--ensure-collections ()
  "Ensure collections are cached."
  (unless (or raindrop-search--collections-ready
              raindrop-search--collections-loading)
    (setq raindrop-search--collections-loading t)
    (raindrop-search--log "loading collections…")
    (raindrop-api-request-async
     "/collections" 'GET nil nil
     (lambda (res err)
       (setq raindrop-search--collections-loading nil)
       (if err
           (progn
             (raindrop-search--log "collections load error: %s" err)
             (setq raindrop-search--collections-ready t)
             (setq raindrop-search--collections-by-title (make-hash-table :test 'equal)
                   raindrop-search--collections-by-id (make-hash-table :test 'eql)))
         (let ((items (or (alist-get 'items res) '())))
           (raindrop-search--collections-build-index items)
           (raindrop-search--log "collections loaded: %d" (hash-table-count raindrop-search--collections-by-id))
           (setq raindrop-search--collections-ready t)))))))

(defun raindrop-search--collection-id-by-title (name)
  "Return collection id for NAME."
  (when (and name raindrop-search--collections-by-title)
    (gethash (downcase name) raindrop-search--collections-by-title)))

(defun raindrop-search--collection-title-by-id (cid)
  "Return collection title for CID."
  (cond
   ((eq cid -1) "Unsorted")
   ((hash-table-p raindrop-search--collections-by-id)
    (gethash cid raindrop-search--collections-by-id))
   (t nil)))

(defun raindrop-search--parse (s)
  "Parse minibuffer string S into plist (:tags :folders :text)."
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
  "Return non-nil if PARSED has searchable content."
  (let ((text (plist-get parsed :text))
        (tags (plist-get parsed :tags))
        (folders (plist-get parsed :folders)))
    (or (and text (>= (length text) 2))
        (and (listp tags) (> (length tags) 0))
        (and (listp folders) (> (length folders) 0)))))

(defun raindrop-search--compose-search (text tags)
  "Return Raindrop search string from TEXT and TAGS."
  (let (parts)
    (when (and (listp tags) tags)
      (push (raindrop--build-tag-search tags 'all) parts))
    (when (and text (not (string-empty-p text)))
      (push text parts))
    (string-join (nreverse parts) " ")))

(defun raindrop-search--endpoint-and-query (collection-id search page)
  "Return (ENDPOINT . QUERY) for COLLECTION-ID, SEARCH, PAGE."
  (let* ((cid (if (numberp collection-id) collection-id raindrop-search-text-collection))
         (pp (max 1 (min raindrop-search-page-size 50)))
         (pg (max 0 (or page 0)))
         (endpoint (format "/raindrops/%d" cid))
         (q (seq-filter
             (lambda (kv) (consp kv))
             (append
              (when (and search (not (string-empty-p search)))
                (list (cons 'search search)))
              (list (cons 'perpage pp)
                    (cons 'page pg)
                    (cons 'expand "collection"))))))
    (cons endpoint q)))

(defun raindrop-search--domain-of (link item)
  "Return domain from LINK, or ITEM's domain field."
  (or (raindrop-search--kv item 'domain)
      (and (stringp link)
           (when (string-match "//\\([^/]+\\)" link)
             (match-string 1 link)))))

(defun raindrop-search--tags->strings (tags)
  "Normalize TAGS to a list of strings."
  (cond
   ((null tags) nil)
   ((vectorp tags) (mapcar (lambda (x) (downcase (format "%s" x))) (append tags nil)))
   ((listp tags)   (mapcar (lambda (x) (downcase (format "%s" x))) tags))
   (t nil)))

(defun raindrop-search--format-candidate (it)
  "Return candidate string in format: [Collection]  excerpt-or-title  #tags  domain."
  (let* ((title   (or (raindrop-search--kv it :title)
                      (raindrop-search--kv it 'title) ""))
         (excerpt (or (raindrop-search--kv it :excerpt)
                      (raindrop-search--kv it 'excerpt) ""))
         (desc    (if (and excerpt (not (string-empty-p excerpt)))
                      excerpt
                    title))
         (desc*   (raindrop-search--truncate (string-trim desc)
                                             raindrop-search-excerpt-max))
         (link    (or (raindrop-search--kv it :link)
                      (raindrop-search--kv it 'link) ""))
         (domain  (raindrop-search--domain-of link it))
         (domain* (when domain (propertize domain 'face 'shadow)))
         (tags    (raindrop-search--tags->strings
                   (or (raindrop-search--kv it :tags)
                       (raindrop-search--kv it 'tags))))
         (tagstr  (when (and tags (listp tags) tags)
                    (mapconcat
                     (lambda (t)
                       (propertize (format "#%s" t)
                                   'face 'raindrop-search-tag))
                     tags " ")))
         (coll-obj (or (raindrop-search--kv it :collection)
                       (raindrop-search--kv it 'collection)))
         (coll-id  (or (raindrop-search--kv it :collectionId)
                       (raindrop-search--kv it 'collectionId)
                       (and (consp coll-obj)
                            (or (raindrop-search--kv coll-obj '$id)
                                (raindrop-search--kv coll-obj 'oid)
                                (raindrop-search--kv coll-obj :$id)
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
    (string-join
     (seq-filter (lambda (x) (and x (not (string-empty-p x))))
                 (list collstr desc* tagstr domain*))
     "  ")))

(defun raindrop-search--apply-results (gen items)
  "Install ITEMS for generation GEN and refresh UI."
  (when (= gen raindrop-search--last-gen)
    (setq raindrop-search--items items)
    (raindrop-search--safe-exhibit)))

(defun raindrop-search--fetch (gen page parsed)
  "Dispatch an async fetch for GEN and PAGE using PARSED input."
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
         (query    (cdr pair))
         (urlstr   (let ((qs (raindrop--build-query query))
                         (base (or (bound-and-true-p raindrop-api-base) "<base>")))
                     (format "%s%s?%s" base endpoint qs))))
    (raindrop-search--log "fetch(gen=%s) search=%S endpoint=%s url=%s"
                          gen (if (string-empty-p search) nil search) endpoint urlstr)
    (raindrop-api-request-async
     endpoint 'GET query nil
     (lambda (res err)
       (when (and err (string-match-p "HTTP 404\\|Network error" err))
         (raindrop-search--log "HTTP treated as empty: %s" err)
         (setq res '((items))))
       (when (= gen raindrop-search--last-gen)
         (if err
             (progn
               (message "Raindrop error: %s" err)
               (raindrop-search--apply-results gen nil))
           (let* ((raw  (or (alist-get 'items res) '()))
                  (norm (mapcar (lambda (x)
                                  (condition-case _e
                                      (raindrop--normalize-item x)
                                    (error x)))
                                raw)))
             (raindrop-search--apply-results gen norm))))))))

(defun raindrop-search--idle-fire ()
  "Idle timer body: parse input and fetch if meaningful."
  (let* ((input (minibuffer-contents-no-properties))
         (parsed (raindrop-search--parse input)))
    (raindrop-search--log "idle fire input=%S" input)
    (when (raindrop-search--meaningful-input-p parsed)
      (setq raindrop-search--last-gen (1+ raindrop-search--last-gen))
      (raindrop-search--fetch raindrop-search--last-gen 0 parsed))))

(defun raindrop-search--schedule ()
  "Schedule the idle search timer; clear stale candidates on input change."
  (let ((input (minibuffer-contents-no-properties)))
    (unless (equal input raindrop-search--last-input)
      (setq raindrop-search--last-input input)
      (setq raindrop-search--items nil)
      (raindrop-search--safe-exhibit)
      (setq raindrop-search--last-gen (1+ raindrop-search--last-gen))))
  (when raindrop-search--timer
    (cancel-timer raindrop-search--timer))
  (setq raindrop-search--timer
        (run-with-idle-timer raindrop-search-idle-delay nil
                             #'raindrop-search--idle-fire)))

(defun raindrop-search--ui-candidates ()
  "Return display strings for current items."
  (mapcar #'raindrop-search--format-candidate
          (or raindrop-search--items '())))

(defun raindrop-search--item-id (it)
  "Return the bookmark numeric id from IT."
  (or (raindrop-search--kv it '_id)
      (raindrop-search--kv it :_id)))

(defun raindrop-search--default-raindrop-url-builder (item)
  "Default Raindrop app URL builder for ITEM.
Tries /my/<collectionId>/item/<id> first, then /my/<collectionId>,
then app root."
  (let* ((cid (or (raindrop-search--kv item :collectionId)
                  (raindrop-search--kv item 'collectionId)))
         (id  (raindrop-search--item-id item)))
    (cond
     ((and (integerp cid) (integerp id))
      (format "https://app.raindrop.io/my/%d/item/%d" cid id))
     ((integerp cid)
      (format "https://app.raindrop.io/my/%d" cid))
     (t "https://app.raindrop.io/"))))

(defun raindrop-search--open-item (item)
  "Open ITEM according to `raindrop-search-enter-action'."
  (pcase raindrop-search-enter-action
    ('raindrop
     (let ((url (funcall raindrop-search-raindrop-url-builder item)))
       (if (and (stringp url) (not (string-empty-p url)))
           (browse-url url)
         (message "No Raindrop URL for this item."))))
    (_
     (let ((url (or (raindrop-search--kv item :link)
                    (raindrop-search--kv item 'link))))
       (if (and (stringp url) (not (string-empty-p url)))
           (browse-url url)
         (message "No link URL for this item."))))))

(defun raindrop-search--exit (cand)
  "Open the selected candidate CAND according to `raindrop-search-enter-action'."
  (let* ((idx (cl-position cand (raindrop-search--ui-candidates)
                           :test #'string=))
         (it (and idx (nth idx raindrop-search--items))))
    (when it
      (raindrop-search--open-item it))))

;;;###autoload
(defun raindrop-search-toggle-enter-action ()
  "Toggle `raindrop-search-enter-action' between 'link and 'raindrop."
  (interactive)
  (setq raindrop-search-enter-action
        (pcase raindrop-search-enter-action
          ('link 'raindrop)
          (_ 'link)))
  (message "raindrop-search: Enter action → %s" raindrop-search-enter-action))

(defun raindrop-search--minibuffer-setup ()
  "Activate live scheduling and ensure collections."
  (when (minibufferp)
    (add-hook 'post-command-hook #'raindrop-search--schedule nil t)
    (raindrop-search--schedule)
    (raindrop-search--ensure-collections)))

(defun raindrop-search--minibuffer-cleanup ()
  "Cleanup hooks and timers."
  (when (minibufferp)
    (remove-hook 'post-command-hook #'raindrop-search--schedule t))
  (when raindrop-search--timer
    (cancel-timer raindrop-search--timer)
    (setq raindrop-search--timer nil)))

;;;###autoload
(defun raindrop-search ()
  "Start interactive Raindrop search with as-you-type completion."
  (interactive)
  (setq raindrop-search--items nil)
  (setq raindrop-search--last-gen 0)
  (minibuffer-with-setup-hook #'raindrop-search--minibuffer-setup
    (unwind-protect
        (let* ((table (lambda (_str _pred action)
                        (pcase action
                          ('metadata '(metadata (category . raindrop)))
                          (_ (raindrop-search--ui-candidates))))))
          (let ((res (completing-read "Raindrop: " table nil t)))
            (when (and res (not (string-empty-p res)))
              (raindrop-search--exit res))))
      (raindrop-search--minibuffer-cleanup))))

(provide 'raindrop-search)

;;; raindrop-search.el ends here
