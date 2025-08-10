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

;;;###autoload
(defun org-babel-execute:raindrop (_body params)
  "Execute a Raindrop org-babel block.
PARAMS is an alist from org-babel.
Recognized params: :tags, :match, :collection, :limit, :output (org-list|org-table)."
  (let* ((tags (raindrop-parse-tags (or (cdr (assoc :tags params))
                                        (alist-get :tags params))))
         (match (or (cdr (assoc :match params)) 'all))
         (limit (or (cdr (assoc :limit params)) raindrop-default-limit))
         (collection (cdr (assoc :collection params)))
         (output (or (cdr (assoc :output params)) 'org-list))
         (items (if tags (apply #'raindrop-fetch (list :tags tags :match match :limit limit :collection (or collection raindrop-default-collection))) '())))
    (pcase output
      ('org-list (if (null items) "- Нет результатов" (raindrop-render-org-list items)))
      (_ (user-error "ob-raindrop: Unsupported :output %S" output)))))

(provide 'ob-raindrop)

;;; ob-raindrop.el ends here
