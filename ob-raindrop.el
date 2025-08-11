;;; ob-raindrop.el --- Org-babel support for raindrop.el -*- lexical-binding: t; -*-

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
;; Org-babel `raindrop` language: returns Org-formatted output suitable for
;; :results raw replace. Parameters mirror dynamic block params.

;;; Code:

(require 'org)
(require 'ob)
(require 'raindrop)
(require 'raindrop-org)

(defvar org-babel-default-header-args:raindrop
  '((:results . "raw replace") (:output . org-list))
  "Default header args for raindrop babel blocks.")

(defun raindrop-ob--param (key params default)
  "Return value for KEY from PARAMS or DEFAULT if not set or empty."
  (let ((v (alist-get key params nil nil #'eq)))
    (if (and v (not (equal v ""))) v default)))

(defun raindrop-ob--items (params)
  "Return fetched Raindrop items based on PARAMS."
  (let* ((tags (raindrop-parse-tags (raindrop-ob--param :tags params nil)))
         (match (or (raindrop-ob--param :match params 'all) 'all))
         (limit (or (raindrop-ob--param :limit params raindrop-default-limit)
                    raindrop-default-limit))
         (collection (or (raindrop-ob--param :collection params raindrop-default-collection)
                         raindrop-default-collection)))
    (if tags
        (apply #'raindrop-fetch
               (list :tags tags :match match :limit limit :collection collection))
      '())))

(defun raindrop-ob--render (items output)
  "Render ITEMS according to OUTPUT format."
  (pcase output
    ('org-list (if (null items) "- No results" (raindrop-render-org-list items)))
    (_ (user-error "ob-raindrop: Unsupported :output %S" output))))

;;;###autoload
(defun org-babel-execute:raindrop (_body params)
  "Execute a Raindrop org-babel block with PARAMS."
  (let* ((output (or (raindrop-ob--param :output params 'org-list) 'org-list))
         (items (raindrop-ob--items params)))
    (raindrop-ob--render items output)))

(provide 'ob-raindrop)

;;; ob-raindrop.el ends here
