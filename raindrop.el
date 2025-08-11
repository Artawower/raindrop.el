;;; raindrop.el --- Raindrop → Org integration -*- lexical-binding: t; -*-

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
;; Raindrop → Org integration utilities and API client.
;; - url.el-based client with Bearer token.
;; - Fetch bookmarks by tags/collection with basic pagination (one page by default).
;; - Shared utilities for renderers and org integrations.

;;; Code:

(require 'url)
(require 'url-http)
(require 'url-cache)
(require 'auth-source)
(require 'json)
(require 'seq)
(require 'subr-x)  ;; string-trim, string-empty-p, when-let, if-let
(require 'rx)

(defgroup raindrop nil
  "Raindrop → Org integration."
  :group 'convenience)

(defcustom raindrop-api-base "https://api.raindrop.io/rest/v1"
  "Base URL for Raindrop REST API."
  :type 'string
  :group 'raindrop)

(defcustom raindrop-auth-source-host "raindrop.io"
  "DEPRECATED: single host name used in auth-source lookup.
Prefer `raindrop-auth-source-hosts`. Still used for backward compatibility."
  :type 'string
  :group 'raindrop)

(defcustom raindrop-auth-source-hosts '("raindrop.io" "api.raindrop.io")
  "Host names used in auth-source to look up the token.
Both plain domain and API subdomain are tried in order."
  :type '(repeat string)
  :group 'raindrop)

(defcustom raindrop-token-source '(auth-source env custom)
  "Ordered sources to retrieve Raindrop token from.
Supported: `auth-source', `env' (RAINDROP_TOKEN), and `custom'."
  :type '(repeat (choice (const auth-source) (const env) (const custom)))
  :group 'raindrop)

(defcustom raindrop-custom-token nil
  "Custom token value if `raindrop-token-source' includes `custom'.
Avoid storing secrets in plain files; prefer auth-source."
  :type '(choice (const :tag "Unset" nil) (string :tag "Token"))
  :group 'raindrop)

(defcustom raindrop-request-timeout 15
  "Timeout in seconds for API requests."
  :type 'integer
  :group 'raindrop)

(defcustom raindrop-default-limit 100
  "Default maximum number of items to fetch (single page up to 100)."
  :type 'integer
  :group 'raindrop)

(defcustom raindrop-default-collection 0
  "Default collection ID used for queries.
Per the Raindrop spec, collectionId=0 means “all collections”. For default
behavior we treat 0 as “all”. Set a concrete numeric ID to query a single
collection."
  :type 'integer
  :group 'raindrop)

(defcustom raindrop-debug-enable nil
  "Enable verbose debug messages for HTTP requests."
  :type 'boolean
  :group 'raindrop)

(defconst raindrop--user-agent
  "raindrop.el/0.1.0 (+https://github.com/artawower/raindrop.el)"
  "User-Agent header for Raindrop requests.")

(defvar raindrop--collections-cache nil
  "Cached list of collections as returned by `/collections` (value of 'items).")

;;;; Small helpers (pure where possible)

(defun raindrop--mask (s)
  "Return masked version of secret string S for logs."
  (when (stringp s)
    (let ((n (length s)))
      (cond
       ((<= n 6) "******")
       (t (concat (substring s 0 3) (make-string (- n 6) ?*) (substring s (- n 3))))))))

(defun raindrop--get-token-from-auth-source ()
  "Retrieve token from auth-source using configured hosts."
  (let* ((hosts (append (and (listp raindrop-auth-source-hosts)
                             raindrop-auth-source-hosts)
                        (list raindrop-auth-source-host)))
         token)
    (catch 'got
      (dolist (h hosts)
        (let* ((found (car (auth-source-search :host h :max 1 :require '(:secret))))
               (secret (when found (plist-get found :secret))))
          (cond
           ((functionp secret) (setq token (funcall secret)))
           ((stringp secret) (setq token secret)))
          (when (and token (stringp token) (> (length token) 0))
            (throw 'got token))))
      nil)))

(defun raindrop--get-token ()
  "Resolve Raindrop token according to `raindrop-token-source'."
  (catch 'done
    (dolist (src raindrop-token-source)
      (pcase src
        ('auth-source
         (let ((tok (raindrop--get-token-from-auth-source)))
           (when (and tok (stringp tok) (> (length tok) 0))
             (throw 'done tok))))
        ('env
         (let ((tok (getenv "RAINDROP_TOKEN")))
           (when (and tok (stringp tok) (> (length tok) 0))
             (throw 'done tok))))
        ('custom
         (when (and raindrop-custom-token
                    (stringp raindrop-custom-token)
                    (> (length raindrop-custom-token) 0))
           (throw 'done raindrop-custom-token)))))
    (user-error "raindrop.el: Token not found. Configure auth-source or RAINDROP_TOKEN.")))

(defun raindrop-parse-tags (tags)
  "Normalize TAGS param to a list of strings.
Accepts list of symbols/strings or a string. The string form supports:
- Comma-separated values: tag1, tag2
- Quoted tokens to allow spaces: `\"file manager\"`, cli
- Whitespace-separated tokens when there are no commas.
Prefer quoting for tags containing spaces or commas."
  (cond
   ((null tags) nil)
   ((listp tags)
    (seq-filter (lambda (s) (and (stringp s) (> (length s) 0)))
                (mapcar (lambda (tag) (if (symbolp tag) (symbol-name tag) tag)) tags)))
   ((stringp tags)
    (let* ((s (string-trim tags))
           (has-comma (string-match-p "," s))
           (re (if has-comma
                   (rx (or (seq "\"" (group (+ (not (any "\"")))) "\"")
                           (+ (not (any ",\t\n\r ")))))
                 (rx (or (seq "\"" (group (+ (not (any "\"")))) "\"")
                         (+ (not (any "\t\n\r ")))))))
           (start 0)
           (out '()))
      (if (string-empty-p s)
          nil
        (while (and (< start (length s)) (string-match re s start))
          (let* ((q (match-string 1 s))
                 (m (match-string 0 s))
                 (tok (or q m)))
            (push (if has-comma
                      (string-trim tok "[, \t\n\r]+" "[, \t\n\r]+")
                    tok)
                  out))
          (setq start (match-end 0)))
        (nreverse out))))
   (t (list (format "%s" tags)))))

;; replace the existing raindrop-parse-folders with this version
(defun raindrop-parse-folders (folders)
  "Normalize FOLDERS to a list of strings.
A single string is preserved verbatim (even if it contains spaces).
If the string contains commas, it is split on commas with quote support.
A list is normalized element-wise."
  (cond
   ((null folders) nil)
   ((listp folders)
    (seq-filter (lambda (s) (and (stringp s) (> (length s) 0)))
                (mapcar (lambda (f) (if (symbolp f) (symbol-name f) f)) folders)))
   ((stringp folders)
    (let* ((s (string-trim folders)))
      (cond
       ((string-empty-p s) nil)
       ((string-match-p "," s)
        (raindrop-parse-tags s))
       (t (list s)))))
   (t (list (format "%s" folders)))))

(defun raindrop--build-query (params)
  "Build URL query string from PARAMS alist ((key . value) ...)."
  (mapconcat (lambda (kv)
               (format "%s=%s"
                       (url-hexify-string (format "%s" (car kv)))
                       (url-hexify-string (format "%s" (cdr kv)))))
             (seq-filter (lambda (kv) (cdr kv)) params)
             "&"))

(defun raindrop--url (endpoint &optional query-alist)
  "Compose full URL for ENDPOINT with QUERY-ALIST."
  (concat (replace-regexp-in-string "/$" "" raindrop-api-base)
          (if (string-prefix-p "/" endpoint) endpoint (concat "/" endpoint))
          (when query-alist
            (concat "?" (raindrop--build-query query-alist)))))

(defun raindrop--collection-endpoint (collection)
  "Return the base endpoint for listing by COLLECTION."
  (let* ((id (cond
              ((numberp collection) collection)
              ((or (eq collection 'all) (eq collection :all) (null collection)) raindrop-default-collection)
              (t raindrop-default-collection))))
    (format "/raindrops/%s" (if (<= id 0) 0 id))))

(defun raindrop--endpoint-for (collection search-present)
  "Pick the correct endpoint given COLLECTION and SEARCH-PRESENT."
  (let* ((id (cond
              ((numberp collection) collection)
              ((or (eq collection 'all) (eq collection :all) (null collection)) raindrop-default-collection)
              (t raindrop-default-collection))))
    (if (and search-present (<= id 0))
        "/raindrops"
      (format "/raindrops/%s" (if (<= id 0) 0 id)))))

(defun raindrop--collections-list ()
  "Return and cache the list of user's collections (list of alists)."
  (or raindrop--collections-cache
      (let* ((payload (raindrop-collections))
             (items (or (alist-get 'items payload)
                        (alist-get 'collections payload)
                        nil)))
        (setq raindrop--collections-cache (and (listp items) items))
        raindrop--collections-cache)))

(defun raindrop--resolve-collection-id (folder-name)
  "Resolve FOLDER-NAME (string/symbol) to a collection ID or nil."
  (let* ((name (if (symbolp folder-name) (symbol-name folder-name) folder-name))
         (items (raindrop--collections-list))
         (match (seq-find (lambda (it)
                            (let ((title (alist-get 'title it)))
                              (and (stringp title)
                                   (or (string= title name)
                                       (string-equal (downcase title) (downcase name))))))
                          items)))
    (alist-get '_id match)))

;;;; HTTP helpers (small, focused)

(defun raindrop--make-headers (method)
  "Return an alist of HTTP headers for METHOD."
  (let ((headers `(("Authorization" . ,(concat "Bearer " (raindrop--get-token)))
                   ("Accept" . "application/json")
                   ("User-Agent" . ,raindrop--user-agent))))
    (if (eq method 'GET)
        headers
      (append headers '(("Content-Type" . "application/json"))))))

(defun raindrop--buffer-http-status ()
  "Extract HTTP status code from current buffer."
  (or (and (boundp 'url-http-response-status) url-http-response-status)
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^HTTP/[^ ]+ \$begin:math:text$[0-9]+\\$end:math:text$" nil t)
          (string-to-number (match-string 1))))))

(defun raindrop--goto-body ()
  "Move point to start of HTTP response body in current buffer."
  (goto-char (point-min))
  (re-search-forward "^$" nil 'move))

(defun raindrop--json-from-point ()
  "Parse JSON from point to buffer end, returning alist or signal user-error."
  (let ((json-object-type 'alist)
        (json-array-type 'list)
        (json-key-type 'symbol))
    (condition-case err
        (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil :false-object :json-false)
      (error (user-error "raindrop.el: JSON parse error: %s" (cadr err))))))

(defun raindrop--error-message-from-body ()
  "Attempt to parse a human-friendly error message from body."
  (let ((body (buffer-substring-no-properties (point) (point-max))))
    (condition-case _
        (let* ((obj (ignore-errors
                      (json-parse-string body :object-type 'alist :array-type 'list :null-object nil :false-object :json-false)))
               (msg (or (alist-get 'message obj)
                        (alist-get 'error obj)
                        (alist-get 'errorMessage obj))))
          (and msg (format "%s" msg)))
      (error nil))))

;;;; Request primitives (sync / async) using helpers

(defun raindrop-api-request (endpoint &optional method query-alist data)
  "Perform HTTP request to Raindrop API (synchronous).
Return parsed JSON as alist. Signal `user-error` for HTTP or parse errors."
  (let* ((method (or method 'GET))
         (url-request-method (upcase (symbol-name method)))
         (url-request-extra-headers (raindrop--make-headers method))
         (url-request-data (and (not (eq method 'GET)) data))
         (url-show-status nil)
         (request-url (raindrop--url endpoint query-alist))
         (buf (let ((url-request-timeout raindrop-request-timeout))
                (when raindrop-debug-enable
                  (message "raindrop.el: [sync] %s %s" url-request-method request-url))
                (url-retrieve-synchronously request-url t t raindrop-request-timeout))))
    (unless (buffer-live-p buf)
      (user-error "raindrop.el: Network error (no buffer)"))
    (unwind-protect
        (with-current-buffer buf
          (let ((status-code (raindrop--buffer-http-status)))
            (raindrop--goto-body)
            (unless (and status-code (<= 200 status-code) (< status-code 300))
              (let ((err-text (raindrop--error-message-from-body)))
                (user-error "raindrop.el: HTTP %s for %s%s"
                            status-code endpoint
                            (if err-text (format ": %s" err-text) ""))))
            (raindrop--json-from-point)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun raindrop-api-request-async (endpoint &optional method query-alist data callback)
  "Asynchronous HTTP request to Raindrop API.
CALLBACK is called as (func RESULT ERR), where only one of RESULT/ERR is non-nil."
  (let* ((method (or method 'GET))
         (url-request-method (upcase (symbol-name method)))
         (url-request-extra-headers (raindrop--make-headers method))
         (url-request-data (and (not (eq method 'GET)) data))
         (url-show-status nil)
         (full-url (raindrop--url endpoint query-alist))
         (cb (or callback #'ignore)))
    (let ((url-request-timeout raindrop-request-timeout))
      (url-retrieve
       full-url
       (lambda (status)
         (let (result err)
           (unwind-protect
               (with-current-buffer (current-buffer)
                 (let ((err-info (plist-get status :error)))
                   (if err-info
                       (setq err (format "Network error: %S" err-info))
                     (let ((status-code (raindrop--buffer-http-status)))
                       (when raindrop-debug-enable
                         (message "raindrop.el: [async] %s %s -> HTTP %s" url-request-method full-url status-code))
                       (raindrop--goto-body)
                       (if (not (and status-code (<= 200 status-code) (< status-code 300)))
                           (let ((err-text (raindrop--error-message-from-body)))
                             (setq err (format "HTTP %s for %s%s"
                                               status-code endpoint
                                               (if err-text (format ": %s" err-text) ""))))
                         (condition-case e
                             (setq result (raindrop--json-from-point))
                           (error (setq err (error-message-string e))))))))
                 (when (buffer-live-p (current-buffer)) (kill-buffer (current-buffer))))
             (funcall cb result err))))
       nil t t))))

;;;; Search building

(defun raindrop--quote-tag (tag)
  "Convert TAG to Raindrop search token: #tag or #\"multi word\"."
  (let ((s (if (symbolp tag) (symbol-name tag) tag)))
    (if (string-match-p "[\t\n\r ]" s)
        (concat "#\"" (replace-regexp-in-string "\"" "\\\"" s) "\"")
      (concat "#" s))))

(defun raindrop--quote-folder (name)
  "Convert folder NAME to token: collection:NAME or collection:\"multi word\"."
  (let ((s (if (symbolp name) (symbol-name name) name)))
    (if (string-match-p "[\t\n\r ]" s)
        (concat "collection:\"" (replace-regexp-in-string "\"" "\\\"" s) "\"")
      (concat "collection:" s))))

(defun raindrop--build-search (tags folders match)
  "Build a combined search string for TAGS and FOLDERS honoring MATCH ('all|'any)."
  (let* ((ts (seq-filter (lambda (s) (and (stringp s) (> (length s) 0)))
                         (mapcar (lambda (tag) (if (symbolp tag) (symbol-name tag) tag)) (or tags '()))))
         (fs (seq-filter (lambda (s) (and (stringp s) (> (length s) 0)))
                         (mapcar (lambda (f) (if (symbolp f) (symbol-name f) f)) (or folders '()))))
         (terms (append (mapcar #'raindrop--quote-tag ts)
                        (mapcar #'raindrop--quote-folder fs))))
    (pcase match
      ('any (string-join (cons "match:OR" terms) " "))
      (_    (string-join terms " ")))))

(defun raindrop--build-tag-search (tags match)
  "Build a search string for TAGS honoring MATCH ('all|'any)."
  (raindrop--build-search tags nil match))

;;;; Normalization

(defun raindrop--normalize-item (raw)
  "Normalize RAW Raindrop item alist into a simpler alist."
  (let* ((link (alist-get 'link raw))
         (title (or (alist-get 'title raw) link))
         (excerpt (or (alist-get 'excerpt raw) ""))
         (tags (alist-get 'tags raw))
         (created (alist-get 'created raw))
         (id (alist-get '_id raw))
         (domain (alist-get 'domain raw))
         (collection (alist-get 'collection raw)))
    (list (cons :id id)
          (cons :link link)
          (cons :title title)
          (cons :excerpt excerpt)
          (cons :tags tags)
          (cons :domain domain)
          (cons :created created)
          (cons :collection collection))))

(defun raindrop--normalize-items (items)
  "Normalize a list of raw ITEMS."
  (mapcar #'raindrop--normalize-item (if (and items (listp items)) items '())))

;;;; Query planning (shared between sync/async)

(defun raindrop--plan (plist)
  "Build a plan from PLIST and return a plist:
:tags :folders :search-present :endpoint :query :coll-id :alt-endpoint"
  (let* ((tags (plist-get plist :tags))
         (folders (or (plist-get plist :folders)
                      (let ((f (plist-get plist :folder))) (and f (list f)))))
         (collection (or (plist-get plist :collection) raindrop-default-collection))
         (limit (or (plist-get plist :limit) raindrop-default-limit))
         (match (or (plist-get plist :match) 'all))
         (tags* (and tags (if (listp tags) tags (list tags))))
         (folders* (and folders (if (listp folders) folders (list folders))))
         (resolved-coll (and folders* (= (length folders*) 1)
                             (let ((cid (raindrop--resolve-collection-id (car folders*))))
                               (and (integerp cid) (> cid 0) cid))))
         (effective-coll (or resolved-coll collection))
         (search (and (or tags* (and folders* (not resolved-coll)))
                      (raindrop--build-search tags* (and (not resolved-coll) folders*) match)))
         (endpoint (raindrop--endpoint-for effective-coll (and search t)))
         (query `((perpage . ,(min limit 100))
                  ,@(when search `((search . ,search)))))
         (coll-id (cond
                   ((numberp effective-coll) effective-coll)
                   ((or (eq effective-coll 'all) (eq effective-coll :all) (null effective-coll)) raindrop-default-collection)
                   (t raindrop-default-collection)))
         (alt (and (<= coll-id 0) search
                   (if (string= endpoint "/raindrops") "/raindrops/0" "/raindrops"))))
    (list :tags tags* :folders folders* :search-present (and search t)
          :endpoint endpoint :query query :coll-id coll-id :alt-endpoint alt)))

;;;; Public fetching API

(defun raindrop-fetch (&rest plist)
  "Fetch raindrops according to PLIST keys:
:tags (list or string), :folders (list/string) or :folder (string),
:collection (number or 0 for all), :limit (int), :sort (symbol), :match ('all|'any).
Returns list of normalized items."
  (when raindrop-debug-enable
    (message "raindrop.el: fetch %S" plist))
  (let* ((plan (raindrop--plan plist))
         (endpoint (plist-get plan :endpoint))
         (query (plist-get plan :query))
         (alt-endpoint (plist-get plan :alt-endpoint))
         payload items)
    (condition-case err
        (setq payload (raindrop-api-request endpoint 'GET query nil)
              items (alist-get 'items payload))
      (user-error
       (let ((msg (error-message-string err)))
         (if (and alt-endpoint (string-match-p "HTTP 404" msg))
             (setq payload (raindrop-api-request alt-endpoint 'GET query nil)
                   items (alist-get 'items payload))
           (signal (car err) (cdr err))))))
    (when (and alt-endpoint (or (null items) (equal items '())))
      (let* ((alt-payload (raindrop-api-request alt-endpoint 'GET query nil))
             (alt-items (alist-get 'items alt-payload)))
        (setq items alt-items)))
    (raindrop--normalize-items items)))

(defun raindrop-fetch-async (plist callback)
  "Asynchronously fetch raindrops according to PLIST and call CALLBACK with (items err)."
  (let* ((plan (raindrop--plan plist))
         (endpoint (plist-get plan :endpoint))
         (query (plist-get plan :query))
         (coll-id (plist-get plan :coll-id))
         (alt-endpoint (plist-get plan :alt-endpoint)))
    (raindrop-api-request-async
     endpoint 'GET query nil
     (lambda (payload err)
       (if err
           ;; If 404 and alt-endpoint exists (all+search), try alt.
           (if (and alt-endpoint (string-match-p "HTTP 404" (format "%s" err)))
               (raindrop-api-request-async
                alt-endpoint 'GET query nil
                (lambda (payload2 err2)
                  (if err2
                      (funcall callback nil err2)
                    (funcall callback (raindrop--normalize-items (alist-get 'items payload2)) nil)))
                )
             (funcall callback nil err))
         (let ((items (alist-get 'items payload)))
           (if (and alt-endpoint (or (null items) (equal items '())))
               (raindrop-api-request-async
                alt-endpoint 'GET query nil
                (lambda (payload3 err3)
                  (if err3
                      (funcall callback nil err3)
                    (funcall callback (raindrop--normalize-items (alist-get 'items payload3)) nil))))
             (funcall callback (raindrop--normalize-items items) nil))))))))

;;;; Filters API

(defun raindrop--filters-endpoint (collection)
  "Return the Filters endpoint for COLLECTION."
  (let* ((id (cond
              ((numberp collection) collection)
              ((or (eq collection 'all) (eq collection :all) (null collection)) raindrop-default-collection)
              (t raindrop-default-collection)))
         (cid (if (<= id 0) 0 id)))
    (format "/filters/%s" cid)))

(defun raindrop-filters (&rest plist)
  "Fetch aggregated filters for a given collection.
PLIST keys:
  :collection — collection id (default `raindrop-default-collection` → “all”)
  :search — search string (same as /raindrops search=)
  :tags-sort — 'count (default) or '_id

Returns an alist per API: fields broken, duplicates, important, notag,
tags (list of {_id count}), types (list of {_id count})."
  (let* ((collection (or (plist-get plist :collection) raindrop-default-collection))
         (search (plist-get plist :search))
         (tags-sort (or (plist-get plist :tags-sort) 'count))
         (endpoint (raindrop--filters-endpoint collection))
         (query `((tagsSort . ,(pcase tags-sort
                                 ('_id "_id")
                                 ('count "count")
                                 (_ "count")))
                  ,@(when (and search (stringp search) (> (length search) 0))
                      `((search . ,search)))))
         (payload (raindrop-api-request endpoint 'GET query nil)))
    payload))

;;;###autoload
(defun raindrop-debug-filters (&optional collection search)
  "Make a raw request to the Filters API for COLLECTION with SEARCH."
  (let* ((coll (or collection raindrop-default-collection)))
    (raindrop-filters :collection coll :search search)))

;;;; Collections helpers

(defun raindrop-collections ()
  "Get the user's collection list (alist payload).
Useful for discovering valid collection IDs for /raindrops/{id} requests."
  (raindrop-api-request "/collections" 'GET nil nil))

;;;###autoload
(defun raindrop-debug-collections ()
  "Return the raw payload with the collection list."
  (raindrop-collections))

;;;###autoload
(defun raindrop-clear-cache ()
  "Clear internal caches (e.g., collections list)."
  (interactive)
  (setq raindrop--collections-cache nil)
  (when raindrop-debug-enable
    (message "raindrop.el: collections cache cleared")))

;;;; Minimal debug helpers

;;;###autoload
(defun raindrop-debug-search (tags &optional collection)
  "Make a raw API request filtered by TAGS and return the payload (alist).
TAGS is a string or list of tags. COLLECTION is a collection id (default 0 = all collections)."
  (let* ((tags-list (raindrop-parse-tags tags))
         (search (and tags-list (raindrop--build-tag-search tags-list 'all)))
         (coll (if (numberp collection) collection 0))
         (endpoint (raindrop--endpoint-for coll (and search t))))
    (raindrop-api-request endpoint 'GET `((perpage . 5) ,@(when search `((search . ,search)))) nil)))

;;;###autoload
(defun raindrop-debug-fetch (tags &optional collection limit)
  "Get a normalized list of bookmarks filtered by TAGS.
COLLECTION is the collection id (default 0 = all). LIMIT is the maximum number of items (default 10)."
  (let* ((tags-list (raindrop-parse-tags tags))
         (coll (if (numberp collection) collection 0))
         (lim (or limit 10)))
    (apply #'raindrop-fetch (list :tags tags-list :collection coll :limit lim :match 'all))))

(provide 'raindrop)

;;; raindrop.el ends here
