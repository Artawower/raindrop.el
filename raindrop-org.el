;;; raindrop-org.el --- Org integration for raindrop.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 artawower

;; Author: artawower <artawower33@gmail.com>
;; URL: https://github.com/artawower/raindrop.el
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Version: 0.1.0
;; Keywords: convenience, outlines, hyperlinks

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Org-facing utilities for raindrop.el.

;;; Code:

(require 'org)
(require 'subr-x)
(require 'seq)
(require 'raindrop)

(defun raindrop-org--debug (fmt &rest args)
  "Log debug message if `raindrop-debug' is enabled."
  (when (bound-and-true-p raindrop-debug)
    (message "raindrop-org: %s" (apply #'format fmt args))))

(defgroup raindrop-org nil
  "Org integration for raindrop.el."
  :group 'raindrop)

(defcustom raindrop-links-empty-text "- No results"
  "Text inserted when no links are found."
  :type 'string
  :group 'raindrop-org)

(defcustom raindrop-heading-tags-match 'all
  "Default matching semantics when extracting tags from a heading."
  :type '(choice (const all) (const any))
  :group 'raindrop-org)

(defcustom raindrop-org-render-tags t
  "If non-nil, append item tags after the link title."
  :type 'boolean
  :group 'raindrop-org)

(defcustom raindrop-org-excerpt-next-line t
  "If non-nil, render excerpt on the next indented line."
  :type 'boolean
  :group 'raindrop-org)

(defcustom raindrop-org-excerpt-indent 3
  "Number of spaces to indent the excerpt line."
  :type 'integer
  :group 'raindrop-org)

(defcustom raindrop-org-smart-grouping-default nil
  "If non-nil, group items under headings automatically in dynamic blocks."
  :type 'boolean
  :group 'raindrop-org)

(defcustom raindrop-org-smart-min-count 2
  "Minimum tag frequency required for a tag to become a heading."
  :type 'integer
  :group 'raindrop-org)

(defcustom raindrop-org-smart-max-groups 8
  "Maximum number of auto-generated groups (headings)."
  :type 'integer
  :group 'raindrop-org)

(defcustom raindrop-org-smart-stop-tags
  '("cli" "terminal" "software" "opensource" "free-software" "awesome-software")
  "Tags considered too generic and excluded from auto-grouping."
  :type '(repeat string)
  :group 'raindrop-org)

(defcustom raindrop-org-smart-heading-level 3
  "Org heading level used for auto-grouping headings."
  :type 'integer
  :group 'raindrop-org)

(defun raindrop-org--log (fmt &rest args)
  "Log a debug message FMT with ARGS when `raindrop-debug-enable' is non-nil."
  (when (bound-and-true-p raindrop-debug-enable)
    (apply #'message (concat "raindrop-org: " fmt) args)))

(defun raindrop-org--sanitize (s)
  "Return S trimmed and with newlines collapsed for list context."
  (when (stringp s)
    (replace-regexp-in-string "\n+" " " (string-trim s))))

(defalias 'raindrop--sanitize-org 'raindrop-org--sanitize)

(defun raindrop-org--format-tags (tags)
  "Return a formatted tags suffix for TAGS or nil when empty."
  (when raindrop-debug-enable
    (raindrop-org--debug "format-tags input=%S (listp=%S vectorp=%S)" tags (listp tags) (vectorp tags)))
  (when (and raindrop-org-render-tags tags)
    (let* ((tag-list (cond
                      ((vectorp tags) (append tags nil))
                      ((listp tags) tags)
                      (t (list tags))))
           (meaningful-tags (seq-filter (lambda (x) (and x (not (string-empty-p (format "%s" x))))) tag-list)))
      (when meaningful-tags
        (let* ((names (mapcar (lambda (tag) (if (symbolp tag) (symbol-name tag) (format "%s" tag))) meaningful-tags))
               (san (mapcar #'raindrop-org--sanitize names)))
          (when (bound-and-true-p raindrop-debug)
            (raindrop-org--debug "formatted tags=%S" (format "  (%s)" (string-join san ", "))))
          (format "  (%s)" (string-join san ", ")))))))

(defun raindrop-render-org-list (items)
  "Render ITEMS as an Org bullet list.
Each item is a plist/alist with keys :link, :title, :excerpt, :tags."
  (mapconcat
   (lambda (it)
     (let* ((link (or (plist-get it :link) (alist-get :link it)))
            (title (or (plist-get it :title) (alist-get :title it) link))
            (excerpt (or (plist-get it :excerpt) (alist-get :excerpt it) ""))
            (tags (or (plist-get it :tags) (alist-get :tags it)))
            (title* (raindrop-org--sanitize title))
            (excerpt* (raindrop-org--sanitize excerpt))
            (tags* (raindrop-org--format-tags tags))
            (head (format "- [[%s][%s]]%s" link title* (or tags* ""))))
       (if (and raindrop-org-excerpt-next-line
                excerpt* (> (length excerpt*) 0))
           (concat head "\n" (make-string raindrop-org-excerpt-indent ?\s) excerpt*)
         head)))
   items
   "\n"))


(defun raindrop-org--parse-tags-string (tags-string)
  "Parse TAGS-STRING with comma separation into tags and excluded tags.
For comma-separated format: \"cli, -openai, emacs\" -> (:tags (\"cli\" \"emacs\") :excluded-tags (\"openai\"))
For single tag with spaces: \"disk usage\" -> (:tags (\"disk usage\") :excluded-tags nil)
For exclusion: \"-disk usage\" -> (:tags nil :excluded-tags (\"disk usage\"))"
  (when (and tags-string (not (string-empty-p tags-string)))
    (let* ((tags '())
           (excluded-tags '())
           ;; Only split by comma, preserve spaces within tags
           (parts (if (string-match-p "," tags-string)
                      (mapcar #'string-trim (split-string tags-string ","))
                    ;; For single tag/value, treat as one item
                    (list (string-trim tags-string))))
           ;; Process each part
           (processed (dolist (part parts)
                        (when (and part (not (string-empty-p part)))
                          (if (string-prefix-p "-" part)
                              (push (string-trim (substring part 1)) excluded-tags)
                            (push part tags))))))
      (list :tags (nreverse tags)
            :excluded-tags (nreverse excluded-tags)))))

(defun raindrop-org--parse-folders-string (folders-string)
  "Parse FOLDERS-STRING with comma separation.
Example: \"folder1,folder2,folder3\" -> (\"folder1\" \"folder2\" \"folder3\")"
  (when (and folders-string (not (string-empty-p folders-string)))
    (mapcar #'string-trim (split-string folders-string ","))))

(defun plist-p (list)
  "Return t if LIST is a valid plist."
  (and (listp list)
       (zerop (mod (length list) 2))
       (cl-every #'keywordp (cl-loop for i from 0 below (length list) by 2
                                     collect (nth i list)))))

(defun raindrop-org--parse-dblock-params (params)
  "Parse org-mode dynamic block PARAMS into a proper plist.
Handles cases where org-mode passes mixed lists like (:name \"raindrop\" :tags cli -openai :match all)."
  (let ((result '())
        (current-key nil)
        (current-values '()))
    ;; Skip :name field if present
    (when (eq (car params) :name)
      (setq params (cddr params)))
    
    (dolist (item params)
      (cond
       ;; If item is a keyword, it's a new key
       ((keywordp item)
        ;; Save previous key-value pair if exists
        (when current-key
          (setq result (plist-put result current-key
                                  (if (= (length current-values) 1)
                                      (car current-values)
                                    (reverse current-values)))))
        ;; Start new key
        (setq current-key item
              current-values '()))
       ;; If item starts with -, it's an exclusion (part of current values)
       ((and (symbolp item) 
             (string-prefix-p "-" (symbol-name item)))
        (when current-key
          (push item current-values)))
       ;; Otherwise it's a value for current key
       (t
        (when current-key
          (push item current-values)))))
    
    ;; Don't forget the last key-value pair
    (when current-key
      (setq result (plist-put result current-key
                              (if (= (length current-values) 1)
                                  (car current-values)
                                (reverse current-values)))))
    result))

(defun raindrop-org--param (params key &optional default)
  "Get KEY from PARAMS (plist or alist). Return DEFAULT when absent."
  (let ((parsed-params 
         (cond
          ;; If params has :name field, it's from org-mode
          ((and (listp params) (eq (car params) :name))
           (raindrop-org--parse-dblock-params params))
          ;; If params looks like mixed format
          ((and (listp params) 
                (keywordp (car params))
                (not (plist-p params)))
           (raindrop-org--parse-dblock-params params))
          ;; Normal plist or alist
          (t params))))
    (let ((v (cond
              ;; If params is a proper plist
              ((and (listp parsed-params) (keywordp (car parsed-params)))
               (plist-get parsed-params key))
              ;; If params is an alist
              ((and (listp parsed-params) (consp (car parsed-params)))
               (alist-get key parsed-params))
              ;; Otherwise try both
              (t (or (ignore-errors (plist-get parsed-params key))
                     (ignore-errors (alist-get key parsed-params)))))))
      (if (eq v nil) default v))))

(defun raindrop-org--normalize-match (match)
  "Normalize MATCH to the symbol 'all or 'any."
  (cond
   ((memq match '(all any)) match)
   ((stringp match)
    (pcase (downcase match) ("any" 'any) (_ 'all)))
   (t 'all)))

(defun raindrop-org--normalize-number (val default)
  "Return numeric VAL or DEFAULT when VAL is not a number-like value."
  (cond
   ((numberp val) val)
   ((and (stringp val) (string-match-p "^[0-9]+$" val)) (string-to-number val))
   (t default)))

(defun raindrop-org--truthy (x)
  "Return non-nil when X represents a true value."
  (cond
   ((eq x t) t)
   ((numberp x) (not (zerop x)))
   ((stringp x)
    (member (downcase (string-trim x)) '("t" "true" "yes" "on" "1")))
   (t (not (null x)))))

(defun raindrop-org--find-begin (subtree-end)
  "Return beginning position of \"#+BEGIN: raindrop\" before SUBTREE-END, or nil."
  (save-excursion
    (when (re-search-forward "^#\\+BEGIN: +raindrop\\b" subtree-end t)
      (match-beginning 0))))

(defun raindrop-org--content-region (block-beg subtree-end)
  "Return content region (BEG . END) for block at BLOCK-BEG within SUBTREE-END."
  (save-excursion
    (goto-char block-beg)
    (forward-line 1)
    (let* ((cbeg (point))
           (found-end (re-search-forward "^#\\+END:" subtree-end t))
           (cend (and found-end (match-beginning 0))))
      (and cbeg cend (cons cbeg cend)))))

(defun raindrop-org--insert-skeleton (tags match)
  "Insert an empty dynamic block with TAGS and MATCH at point."
  (let ((tags-string (if tags
                         (mapconcat (lambda (tag)
                                      (if (string-prefix-p "-" tag)
                                          tag
                                        tag))
                                    tags ",")
                       "")))
    (insert (format "#+BEGIN: raindrop :tags \"%s\" :match %s\n#+END:\n"
                    tags-string match))))

(defun raindrop-org--replace-content (region text)
  "Replace REGION (cons BEG . END) with TEXT and a trailing newline."
  (when (and region (consp region))
    (delete-region (car region) (cdr region))
    (goto-char (car region))
    (insert text)
    (insert "\n")))

(defun raindrop-org--heading-marker ()
  "Return a buffer position marker for the current heading."
  (save-excursion
    (org-back-to-heading t)
    (point-marker)))

(defun raindrop-org--subtree-end ()
  "Return end position of the current subtree."
  (save-excursion
    (org-end-of-subtree t t)))

(defun raindrop-org--ensure-block (tags match)
  "Ensure a raindrop dynamic block exists under current heading.
Return plist with :block-beg, :subtree-end and :region."
  (org-end-of-meta-data t)
  (let* ((beg (point))
         (subtree-end (raindrop-org--subtree-end))
         (block-beg (raindrop-org--find-begin subtree-end))
         region)
    (unless block-beg
      (goto-char beg)
      (raindrop-org--insert-skeleton tags match)
      (setq subtree-end (raindrop-org--subtree-end))
      (save-excursion
        (goto-char beg)
        (setq block-beg (raindrop-org--find-begin subtree-end))))
    (setq region (raindrop-org--content-region block-beg subtree-end))
    (list :block-beg block-beg :subtree-end subtree-end :region region)))

(defun raindrop-org--with-region-at-heading (org-buf heading-marker fn)
  "Call FN with the content region for the raindrop block at HEADING-MARKER in ORG-BUF."
  (when (buffer-live-p org-buf)
    (with-current-buffer org-buf
      (save-restriction
        (widen)
        (save-excursion
          (when (and (markerp heading-marker) (marker-position heading-marker))
            (goto-char (marker-position heading-marker)))
          (let* ((subtree-end (raindrop-org--subtree-end))
                 (block-beg (raindrop-org--find-begin subtree-end))
                 (region (and block-beg (raindrop-org--content-region block-beg subtree-end))))
            (when region (funcall fn region))))))))

(defun raindrop-org--render-result (items err)
  "Return rendered text for ITEMS or an error ERR."
  (cond
   (err (format "- Error: %s" err))
   ((null items) raindrop-links-empty-text)
   (t (raindrop-render-org-list items))))

(defun raindrop-extract-heading-tags ()
  "Return a list of tags from the current Org heading, or nil."
  (save-excursion
    (org-back-to-heading t)
    (org-get-tags nil t)))

;; smart auto-grouping

(defun raindrop-org--norm-tag (tag)
  "Normalize TAG to a lowercased string."
  (downcase (string-trim (format "%s" tag))))

(defun raindrop-org--item-tags-norm (item)
  "Return a normalized unique tag list for ITEM."
  (let* ((tags (or (plist-get item :tags) (alist-get :tags item))))
    (seq-uniq (mapcar #'raindrop-org--norm-tag (or tags '())))))

(defun raindrop-org--tag-frequencies (items)
  "Return a hash table of tag frequencies across ITEMS."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (it items)
      (dolist (tg (raindrop-org--item-tags-norm it))
        (puthash tg (1+ (gethash tg h 0)) h)))
    h))

(defun raindrop-org--eligible-tags (freqs &optional exclude-tags exclude-groups)
  "Return a list of heading tags from FREQS filtered and sorted by frequency.
EXCLUDE-TAGS is a list of search tags to exclude from heading selection.
EXCLUDE-GROUPS is a list of user-specified tags to exclude from grouping."
  (let* ((stop (let ((s (make-hash-table :test 'equal)))
                 (dolist (stop-tag raindrop-org-smart-stop-tags)
                   (puthash (raindrop-org--norm-tag stop-tag) t s))
                 ;; Add excluded search tags to stop list
                 (when exclude-tags
                   (dolist (ex-tag exclude-tags)
                     (puthash (raindrop-org--norm-tag ex-tag) t s)))
                 ;; Add user-specified exclude groups to stop list  
                 (when exclude-groups
                   (dolist (ex-group exclude-groups)
                     (puthash (raindrop-org--norm-tag ex-group) t s)))
                 s))
         (all '()))
    (maphash
     (lambda (tag cnt)
       (when (and (>= cnt raindrop-org-smart-min-count)
                  (not (gethash tag stop)))
         (push (cons tag cnt) all)))
     freqs)
    (mapcar #'car
            (seq-take
             (sort all (lambda (a b)
                         (if (/= (cdr a) (cdr b))
                             (> (cdr a) (cdr b))
                           (string-lessp (car a) (car b)))))
             raindrop-org-smart-max-groups))))

(defun raindrop-org--choose-primary-tag (item selected-tags freqs)
  "Pick primary tag for ITEM among SELECTED-TAGS using FREQS as tie-breaker."
  (let* ((itags (raindrop-org--item-tags-norm item))
         (cands (seq-filter (lambda (t) (member t selected-tags)) itags)))
    (car (seq-sort
          (lambda (a b)
            (let ((fa (gethash a freqs 0))
                  (fb (gethash b freqs 0)))
              (if (/= fa fb)
                  (> fa fb)
                (< (seq-position selected-tags a)
                   (seq-position selected-tags b)))))
          cands))))

(defun raindrop-org--capitalize (s)
  "Capitalize the first character of S."
  (if (and s (> (length s) 0))
      (concat (upcase (substring s 0 1)) (substring s 1))
    s))

(defun raindrop-org--group-items-auto (items &optional exclude-tags exclude-groups)
  "Return an alist of (Heading . items) using frequency-based auto-grouping.
EXCLUDE-TAGS is a list of search tags to exclude from heading selection.
EXCLUDE-GROUPS is a list of user-specified tags to exclude from grouping."
  (let* ((freqs (raindrop-org--tag-frequencies items))
         (selected (raindrop-org--eligible-tags freqs exclude-tags exclude-groups))
         (table (make-hash-table :test 'equal)))
    (dolist (tag selected) (puthash tag '() table))
    (puthash "Other" '() table)
    (dolist (it items)
      (let ((tag (raindrop-org--choose-primary-tag it selected freqs)))
        (puthash (or tag "Other")
                 (cons it (gethash (or tag "Other") table))
                 table)))
    (append
     (mapcar (lambda (tg) (cons (raindrop-org--capitalize tg)
                                (nreverse (gethash tg table))))
             selected)
     (list (cons "Other" (nreverse (gethash "Other" table)))))))

(defun raindrop-org--get-context-heading-level ()
  "Determine appropriate heading level based on current org context.
Returns the level that group headings should use."
  (if (and (eq major-mode 'org-mode)
           (buffer-file-name))  ; Only in actual org files, not temp buffers
      (save-excursion
        (let ((current-level 0))
          ;; Search backwards for the nearest heading
          (while (and (not (bobp)) (= current-level 0))
            (forward-line -1)
            (when (looking-at org-heading-regexp)
              (setq current-level (org-current-level))))
          ;; If we found a heading, use next level; otherwise use level 1
          (if (> current-level 0) (1+ current-level) 1)))
    ;; Fallback to configured level for tests and non-org contexts
    raindrop-org-smart-heading-level))

(defun raindrop-org--render-grouped (grouped)
  "Render GROUPED (alist of name . items) as headings and lists."
  (let* ((lvl (raindrop-org--get-context-heading-level))
         (stars (make-string lvl ?*)))
    (mapconcat
     (lambda (pair)
       (let ((name (car pair))
             (items (cdr pair)))
         (when (and items (> (length items) 0))
           (concat stars " " name "\n"
                   (raindrop-render-org-list items) "\n"))))
     grouped "")))

;; dynamic block

(defun org-dblock-write:raindrop (params)
  "Writer for the \"raindrop\" dynamic block using PARAMS.
Supported formats:
  :tags \"cli,-openai,emacs\"    - comma-separated tags with exclusions
  :folders \"work,personal\"      - comma-separated folders
  :search \"query text\"          - text search query
  :exclude-groups \"cli,terminal\" - tags to exclude from smart grouping
  :match all/any                 - matching mode
  :limit 20                      - number limit
  :collection 0                  - collection ID
  :smart t                       - enable smart grouping"
  (raindrop-org--debug "dblock params=%S" params)
  
  (let* ((tags-raw (plist-get params :tags))
         ;; Convert tags to string format
         (tags-string (cond
                       ((stringp tags-raw) tags-raw)
                       ((listp tags-raw) 
                        (mapconcat (lambda (x) (format "%s" x)) tags-raw ","))
                       ((symbolp tags-raw) (symbol-name tags-raw))
                       (t nil)))
         ;; Parse tags and excluded tags
         (parsed-tags (raindrop-org--parse-tags-string tags-string))
         (tags (plist-get parsed-tags :tags))
         (excluded-tags (plist-get parsed-tags :excluded-tags))
         
         ;; Parse folders
         (folders-raw (or (plist-get params :folders)
                          (plist-get params :folder)))
         (folders-string (cond
                          ((stringp folders-raw) folders-raw)
                          ((listp folders-raw)
                           (mapconcat (lambda (x) (format "%s" x)) folders-raw ","))
                          ((symbolp folders-raw) (symbol-name folders-raw))
                          (t nil)))
         (folders (raindrop-org--parse-folders-string folders-string))
         
         ;; Parse search text
         (search-text (plist-get params :search))
         
         ;; Parse exclude-groups
         (exclude-groups-raw (plist-get params :exclude-groups))
         (exclude-groups-string (cond
                                 ((stringp exclude-groups-raw) exclude-groups-raw)
                                 ((listp exclude-groups-raw)
                                  (mapconcat (lambda (x) (format "%s" x)) exclude-groups-raw ","))
                                 ((symbolp exclude-groups-raw) (symbol-name exclude-groups-raw))
                                 (t nil)))
         (exclude-groups (raindrop-org--parse-folders-string exclude-groups-string))
         
         ;; Other parameters - FIX: wrap default value in quotes
         (match (raindrop-org--normalize-match 
                 (or (plist-get params :match) 'all)))
         (collection* (raindrop-org--normalize-number
                       (plist-get params :collection)
                       raindrop-default-collection))
         (limit (raindrop-org--normalize-number
                 (or (plist-get params :limit) raindrop-default-limit)
                 raindrop-default-limit))
         (smart-raw (or (plist-get params :smart) raindrop-org-smart-grouping-default))
         (smart-flag (raindrop-org--truthy smart-raw)))
    
    (raindrop-org--debug "parsed: tags=%S excluded-tags=%S folders=%S" 
                         tags excluded-tags folders)
    
    ;; Fetch items
    (let* ((items (if (or tags excluded-tags folders search-text)
                      (condition-case err
                          (let ((fetch-args (append 
                                             (list :match match 
                                                   :limit limit 
                                                   :collection collection*)
                                             (when tags 
                                               (list :tags tags))
                                             (when excluded-tags 
                                               (list :excluded-tags excluded-tags))
                                             (when folders 
                                               (list :folders folders))
                                             (when (and search-text (not (string-empty-p (string-trim search-text))))
                                               (list :search search-text)))))
                            (raindrop-org--debug "fetch-args=%S" fetch-args)
                            (apply #'raindrop-fetch fetch-args))
                        (error 
                         (raindrop-org--debug "fetch error=%S" err)
                         (message "Raindrop fetch error: %s" (error-message-string err))
                         nil))
                    '()))
           ;; Render content
           (content
            (cond
             ((or (null items) (equal items '())) 
              raindrop-links-empty-text)
             (smart-flag
              (raindrop-org--render-grouped 
               (raindrop-org--group-items-auto items tags exclude-groups)))
             (t 
              (raindrop-render-org-list items)))))
      
      ;; Insert content
      (let ((content-beg (point)))
        (when (re-search-forward "^#\\+END:" nil t)
          (delete-region content-beg (match-beginning 0)))
        (goto-char content-beg)
        (insert content)
        (unless (bolp) (insert "\n"))))))

;;;###autoload
(defun raindrop-insert-or-update-links-under-heading (&optional use-any)
  "Insert or refresh a raindrop dynamic block under the current heading.
With optional prefix USE-ANY, use OR semantics for heading tags."
  (interactive "P")
  (let* ((tags (raindrop-extract-heading-tags))
         (match (if use-any 'any raindrop-heading-tags-match)))
    (unless (and tags (> (length tags) 0))
      (user-error "raindrop-org: Current heading has no tags"))
    (let* ((org-buf (current-buffer))
           (heading-marker (raindrop-org--heading-marker))
           ;; Convert heading tags to comma-separated format
           (tags-string (mapconcat #'identity tags ",")))
      (save-excursion
        (goto-char (marker-position heading-marker))
        (let* ((info (raindrop-org--ensure-block tags match)))
          (raindrop-org--replace-content (plist-get info :region) "- Loading…")))
      (let* ((parsed (raindrop-org--parse-tags-string tags-string))
             (search-tags (plist-get parsed :tags)))
        (raindrop-search-bookmarks 
         (mapconcat (lambda (tag) (concat "#" tag)) search-tags " ")
         (lambda (items err)
           (raindrop-org--with-region-at-heading
            org-buf heading-marker
            (lambda (region)
              (raindrop-org--replace-content
               region
               (raindrop-org--render-result items err)))))
         raindrop-default-limit))
      (when (markerp heading-marker) (set-marker heading-marker nil)))))

(provide 'raindrop-org)

;;; raindrop-org.el ends here
