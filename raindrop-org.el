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
    (message "raindrop-org: format-tags input=%S (listp=%S vectorp=%S)" tags (listp tags) (vectorp tags)))
  (when (and raindrop-org-render-tags tags)
    (let* ((tag-list (cond
                      ((vectorp tags) (append tags nil))
                      ((listp tags) tags)
                      (t (list tags))))
           (meaningful-tags (seq-filter (lambda (x) (and x (not (string-empty-p (format "%s" x))))) tag-list)))
      (when meaningful-tags
        (let* ((names (mapcar (lambda (tag) (if (symbolp tag) (symbol-name tag) (format "%s" tag))) meaningful-tags))
               (san (mapcar #'raindrop-org--sanitize names)))
          (when raindrop-debug-enable
            (message "raindrop-org: formatted tags=%S" (format "  (%s)" (string-join san ", "))))
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

(defun raindrop-org--param (params key &optional default)
  "Get KEY from PARAMS (plist or alist). Return DEFAULT when absent."
  (let ((v (or (plist-get params key) (alist-get key params))))
    (if (eq v nil) default v)))

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
  (insert (format "#+BEGIN: raindrop :tags %s :match %s :output org-list\n#+END:\n"
                  (mapconcat #'identity (or tags '()) ",")
                  match)))

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

(defun raindrop-org--eligible-tags (freqs)
  "Return a list of heading tags from FREQS filtered and sorted by frequency."
  (let* ((stop (let ((s (make-hash-table :test 'equal)))
                 (dolist (stop-tag raindrop-org-smart-stop-tags)
                   (puthash (raindrop-org--norm-tag stop-tag) t s))
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

(defun raindrop-org--group-items-auto (items)
  "Return an alist of (Heading . items) using frequency-based auto-grouping."
  (let* ((freqs (raindrop-org--tag-frequencies items))
         (selected (raindrop-org--eligible-tags freqs))
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

(defun raindrop-org--render-grouped (grouped)
  "Render GROUPED (alist of name . items) as headings and lists."
  (let* ((lvl (max 1 raindrop-org-smart-heading-level))
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
  "Writer for the \"raindrop\" dynamic block using PARAMS."
  (message "raindrop-org: dblock params=%S" params)
  (let* ((tags-raw (raindrop-org--param params :tags))
         (tags (raindrop-parse-tags tags-raw)))
    (message "raindrop-org: tags-raw=%S tags=%S" tags-raw tags)
    (let* ((folders (raindrop-parse-folders
                     (or (raindrop-org--param params :folders)
                         (raindrop-org--param params :folder))))
           (match (raindrop-org--normalize-match (raindrop-org--param params :match 'all)))
           (collection* (raindrop-org--normalize-number
                         (raindrop-org--param params :collection)
                         raindrop-default-collection))
           (limit (raindrop-org--normalize-number
                   (raindrop-org--param params :limit raindrop-default-limit)
                   raindrop-default-limit))
           (items (if (or tags folders)
                      (progn
                        (message "raindrop-org: calling raindrop-fetch with tags=%S folders=%S match=%S limit=%S collection=%S"
                                 tags folders match limit collection*)
                        (let ((fetch-args (append (list :match match :limit limit :collection collection*)
                                                  (when tags (list :tags tags))
                                                  (when folders (list :folders folders)))))
                          (message "raindrop-org: fetch-args=%S" fetch-args)
                          (condition-case err
                              (let ((result (apply #'raindrop-fetch fetch-args)))
                                (message "raindrop-org: fetch result length=%S" (length result))
                                (message "raindrop-org: first result=%S" (car result))
                                result)
                            (error 
                             (message "raindrop-org: fetch error=%S" err)
                             '()))))
                    (progn
                      (message "raindrop-org: no tags or folders, returning empty list")
                      '())))
           (smart-raw (raindrop-org--param params :smart raindrop-org-smart-grouping-default))
           (smart-flag (raindrop-org--truthy smart-raw))
           (content
            (cond
             ((or (null items) (equal items '())) raindrop-links-empty-text)
             (smart-flag
              (raindrop-org--render-grouped (raindrop-org--group-items-auto items)))
             (t (raindrop-render-org-list items)))))
      (raindrop-org--log "dblock params tags=%S folders=%S match=%s coll=%s limit=%s smart=%s"
                         tags folders match collection* limit smart-flag)
      (let ((content-beg (point)))
        (when (re-search-forward "^#\\+END:" nil t)
          (delete-region content-beg (match-beginning 0)))
        (goto-char content-beg)
        (insert content)
        (insert "\n")))))

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
           (heading-marker (raindrop-org--heading-marker)))
      (save-excursion
        (goto-char (marker-position heading-marker))
        (let* ((info (raindrop-org--ensure-block tags match)))
          (raindrop-org--replace-content (plist-get info :region) "- Loading…")))
      (let ((input (string-join
                    (mapcar (lambda (tag) (concat "#" tag)) tags)
                    " ")))
        (raindrop-search-bookmarks 
         input
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
