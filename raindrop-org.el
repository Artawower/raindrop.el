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
;; Org-facing utilities for raindrop.el:
;; - Dynamic block writer `raindrop` that inserts org list of links.
;; - Helpers to derive tags from current heading and update content inline.

;;; Code:

(require 'org)
(require 'subr-x)
(require 'raindrop)

(defgroup raindrop-org nil
  "Org integration for raindrop.el."
  :group 'raindrop)

(defcustom raindrop-links-empty-text "- No results"
  "Text inserted when no links are found."
  :type 'string
  :group 'raindrop-org)

(defcustom raindrop-heading-tags-match 'all
  "Default matching semantics for heading tags.
Either 'all (AND) or 'any (OR)."
  :type '(choice (const all) (const any))
  :group 'raindrop-org)

(defun raindrop--sanitize-org (s)
  "Sanitize string S for Org export in list context."
  (when (stringp s)
    (replace-regexp-in-string "\n+" " " (string-trim s))))

(defun raindrop-render-org-list (items)
  "Return an Org bullet list string for ITEMS.
ITEM is an alist with keys :link :title :excerpt."
  (mapconcat
   (lambda (it)
     (let* ((link (or (plist-get it :link) (alist-get :link it)))
            (title (or (plist-get it :title) (alist-get :title it) link))
            (excerpt (or (plist-get it :excerpt) (alist-get :excerpt it) ""))
            (title* (raindrop--sanitize-org title))
            (excerpt* (raindrop--sanitize-org excerpt)))
       (format "- [[%s][%s]]%s" link title* (if (and excerpt* (> (length excerpt*) 0))
                                                (format " — %s" excerpt*)
                                              ""))))
   items
   "\n"))

;;;###autoload
(defun raindrop--param (params key &optional default)
  "Get KEY from PARAMS supporting both plist and alist forms."
  (let ((v (or (plist-get params key)
               (alist-get key params))))
    (if (eq v nil) default v)))

(defun org-dblock-write:raindrop (params)
  "Dynamic block writer for Raindrop.
PARAMS plist:
  :tags (string or list) — tags to match
  :folders or :folder — folder names to match (Raindrop collections)
  :match ('all|'any) — semantics (default 'all)
  :collection (number) — optional collection id
  :limit (int) — max items (default `raindrop-default-limit`)"
  (let* ((tags (raindrop-parse-tags (raindrop--param params :tags)))
         (folders (raindrop-parse-folders (or (raindrop--param params :folders)
                                             (raindrop--param params :folder))))
         (match-raw (raindrop--param params :match 'all))
         (match (cond
                 ((symbolp match-raw) match-raw)
                 ((stringp match-raw)
                  (pcase (downcase match-raw)
                    ("any" 'any)
                    (_ 'all)))
                 (t 'all)))
         (collection (raindrop--param params :collection))
         (collection* (cond
                       ((numberp collection) collection)
                       ((and (stringp collection) (string-match-p "^[0-9]+$" collection))
                        (string-to-number collection))
                       (t raindrop-default-collection)))
         (limit (let ((l (raindrop--param params :limit raindrop-default-limit)))
                  (cond
                   ((numberp l) l)
                   ((and (stringp l) (string-match-p "^[0-9]+$" l)) (string-to-number l))
                   (t raindrop-default-limit))))
         (items (if (or tags folders)
                    (apply #'raindrop-fetch (append (list :match match :limit limit :collection collection*)
                                                    (when tags (list :tags tags))
                                                    (when folders (list :folders folders))))
                  '())))

    (message "items: %s" items)
    (let ((content-beg (point)))
      (when (re-search-forward "^#\\+END:" nil t)
        (delete-region content-beg (match-beginning 0)))
      (goto-char content-beg)
      (insert (if (or (null items) (equal items '()))
                  raindrop-links-empty-text
                (raindrop-render-org-list items)))
      (insert "\n"))))

;;;###autoload
(defun raindrop-extract-heading-tags ()
  "Return list of tags from current Org heading or nil."
  (save-excursion
    (org-back-to-heading t)
    (org-get-tags nil t)))

;;;###autoload
(defun raindrop-insert-or-update-links-under-heading (&optional use-any)
  "Insert or refresh Raindrop links list under current heading body.
With prefix argument USE-ANY (C-u), use OR semantics for tags."
  (interactive "P")
  (let* ((tags (raindrop-extract-heading-tags))
         (match (if use-any 'any raindrop-heading-tags-match)))
    (unless (and tags (> (length tags) 0))
      (user-error "raindrop.el: Current heading has no tags"))
    (let* ((org-buf (current-buffer))
           (heading-marker (save-excursion (org-back-to-heading t) (point-marker))))
      (save-excursion
        (goto-char heading-marker)
        (org-end-of-meta-data t)
        (let* ((beg (point)) (subtree-end (save-excursion (org-end-of-subtree t t))) found)
          (save-excursion
            (setq found (and (re-search-forward "^#\+BEGIN: +raindrop\b" subtree-end t)
                             (match-beginning 0))))
          (if found
              (goto-char found)
            (goto-char beg)
            (insert (format "#+BEGIN: raindrop :tags %s :match %s :output org-list\n#+END:\n"
                            (mapconcat #'identity tags ",") match))))
        ;; Placeholder
        (save-excursion
          (let* ((subtree-end (save-excursion (org-end-of-subtree t t)))
                 (found-beg (re-search-forward "^#\\+BEGIN: +raindrop\\b" subtree-end t)))
            (when found-beg
              (forward-line 1)
              (let* ((cbeg (point))
                     (found-end (re-search-forward "^#\\+END:" subtree-end t))
                     (cend (and found-end (match-beginning 0))))
                (when (and cbeg cend (>= cend cbeg)
                           (<= (point-min) cbeg) (<= cend (point-max)))
                  (delete-region cbeg cend)
                  (goto-char cbeg)
                  (insert "- Loading…\n"))))))
        ;; Async fetch and replace within original org buffer
        (prog1 :started
          (raindrop-fetch-async
           (list :tags tags :match match :collection raindrop-default-collection :limit raindrop-default-limit)
           (lambda (items err)
             (when (buffer-live-p org-buf)
               (with-current-buffer org-buf
                 (save-restriction
                   (widen)
                   (save-excursion
                     (when (and (markerp heading-marker) (marker-position heading-marker))
                       (goto-char heading-marker))
                     (org-end-of-meta-data t)
                     (let* ((subtree-end (save-excursion (org-end-of-subtree t t)))
                            (found-beg (re-search-forward "^#\\+BEGIN: +raindrop\\b" subtree-end t)))
                       (when found-beg
                         (forward-line 1)
                         (let* ((cbeg (point))
                                (found-end (re-search-forward "^#\\+END:" subtree-end t))
                                (cend (and found-end (match-beginning 0))))
                           (when (and cbeg cend (>= cend cbeg)
                                      (<= (point-min) cbeg) (<= cend (point-max)))
                             (delete-region cbeg cend)
                             (goto-char cbeg)
                             (insert (cond
                                      (err (format "- Error: %s" err))
                                      ((null items) raindrop-links-empty-text)
                                      (t (raindrop-render-org-list items))))
                             (insert "\n"))))))))
               (when (markerp heading-marker)
                 (set-marker heading-marker nil))))))))))
(provide 'raindrop-org)

;;; raindrop-org.el ends here
