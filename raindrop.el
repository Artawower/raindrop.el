;;; raindrop.el --- Raindrop → Org integration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 artawower

;; Author: artawower <artawower33@gmail.com>
;; URL: https://github.com/artawower/raindrop.el
;; Package-Requires: ((emacs "27.1") (org "9.4") (request "0.3.0"))
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

(require 'request)
(require 'auth-source)
(require 'json)
(require 'seq)
(require 'subr-x)  ;; string-trim, string-empty-p, when-let, if-let
(require 'rx)
(require 'cl-lib)

(defgroup raindrop nil
  "Raindrop → Org integration."
  :group 'convenience)

(defcustom raindrop-debug nil
  "Enable debug messages for raindrop operations."
  :type 'boolean
  :group 'raindrop)

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

(defcustom raindrop-debug-enable t
  "Enable verbose debug messages for HTTP requests."
  :type 'boolean
  :group 'raindrop)

(defconst raindrop--user-agent
  "raindrop.el/0.1.0 (+https://github.com/artawower/raindrop.el)"
  "User-Agent header for Raindrop requests.")

(defvar raindrop--collections-cache nil
  "Cached list of collections as returned by `/collections` (value of 'items).")

;;;; Small helpers (pure where possible)

(defun raindrop--debug (fmt &rest args)
  "Log debug message if `raindrop-debug' is enabled."
  (when raindrop-debug
    (message "raindrop.el: %s" (apply #'format fmt args))))

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

(defun raindrop-parse-tags-with-exclusion (tags)
  "Parse TAGS into included and excluded tags.
Supports '-tag' syntax for exclusion in string form.
Returns plist with :tags and :excluded-tags keys."
  (let ((parsed-tags (raindrop-parse-tags tags)))
    (if (not (stringp tags))
        ;; For non-string input, no exclusion syntax
        (list :tags parsed-tags :excluded-tags nil)
      ;; For string input, separate excluded tags
      (let ((included '())
            (excluded '()))
        (dolist (tag parsed-tags)
          (if (string-prefix-p "-" tag)
              (push (substring tag 1) excluded)
            (push tag included)))
        (list :tags (nreverse included) 
              :excluded-tags (nreverse excluded))))))

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

;;;; Request-based HTTP implementation

(defun raindrop--make-headers (method)
  "Return an alist of HTTP headers for METHOD."
  (let ((headers `(("Authorization" . ,(concat "Bearer " (raindrop--get-token)))
                   ("Accept" . "application/json")
                   ("User-Agent" . ,raindrop--user-agent))))
    (if (eq method 'GET)
        headers
      (append headers '(("Content-Type" . "application/json"))))))


;;;; Request-based HTTP primitives

(defun raindrop-api-request (endpoint &optional method query-alist data)
  "Perform HTTP request to Raindrop API (synchronous).
Return parsed JSON as alist. Signal `user-error` for HTTP or parse errors."
  (let* ((method (or method 'GET))
         (url (concat raindrop-api-base endpoint))
         (token (raindrop--get-token))
         (headers `(("Authorization" . ,(concat "Bearer " token))
                    ("Content-Type" . "application/json")))
         (json-data (when data (json-encode data)))
         (type (pcase method ('GET "GET") ('PUT "PUT") ('POST "POST") ('DELETE "DELETE") (_ "GET")))
         (result-data nil)
         (result-error nil))
    (when raindrop-debug
      (raindrop--debug "[sync] %s %s" type url))
    (request url
      :type type
      :headers headers
      :params query-alist
      :data json-data
      :parser 'json-read
      :encoding 'utf-8
      :timeout raindrop-request-timeout
      :sync t
      :success (cl-function (lambda (&key data &allow-other-keys)
                              (setq result-data data)))
      :error (cl-function (lambda (&key error-thrown response &allow-other-keys)
                            (let ((status-code (when response (request-response-status-code response)))
                                  (err-msg (format "%s" error-thrown)))
                              (setq result-error (format "HTTP %s for %s: %s"
                                                        (or status-code "unknown") endpoint err-msg))))))
    (if result-error
        (user-error "raindrop.el: %s" result-error)
      result-data)))

(defun raindrop-api-request-async (endpoint &optional method query-alist data callback)
  "Asynchronous HTTP request to Raindrop API.
CALLBACK is called as (func RESULT ERR), where only one of RESULT/ERR is non-nil."
  (let* ((method (or method 'GET))
         (url (concat raindrop-api-base endpoint))
         (token (raindrop--get-token))
         (headers `(("Authorization" . ,(concat "Bearer " token))
                    ("Content-Type" . "application/json")))
         (json-data (when data (json-encode data)))
         (type (pcase method ('GET "GET") ('PUT "PUT") ('POST "POST") ('DELETE "DELETE") (_ "GET")))
         (cb (or callback #'ignore)))
    (when raindrop-debug
      (raindrop--debug "[async] %s %s" type url))
    (request url
      :type type 
      :headers headers 
      :params query-alist 
      :data json-data
      :parser 'json-read 
      :encoding 'utf-8
      :success (cl-function (lambda (&key data &allow-other-keys)
                              (funcall cb data nil)))
      :error   (cl-function (lambda (&key error-thrown &allow-other-keys)
                              (funcall cb nil (format "%s" error-thrown)))))))

;;;; Search parsing and query building

(defun raindrop--parse-search-input (s)
  "Parse search input S into tags, folders, excluded-tags, and text components.
Returns plist with :tags :excluded-tags :folders :text keys.
Supports '#tag', '#\"spaced tag\"', '-#tag', '-#\"spaced tag\"' for tags."
  (let* ((s (or s ""))
         (tags '())
         (excluded-tags '())
         (folders '())
         (texts '())
         (pos 0)
         (len (length s)))
    ;; Parse character by character to handle quoted tags
    (while (< pos len)
      (let ((char (aref s pos)))
        (cond
         ;; Skip whitespace
         ((memq char '(?\s ?\t))
          (setq pos (1+ pos)))
         
         ;; Handle excluded tags (-#tag or -#"quoted tag")
         ((and (< pos (- len 2)) (string= (substring s pos (+ pos 2)) "-#"))
          (setq pos (+ pos 2))
          (if (and (< pos len) (= (aref s pos) ?\"))
              ;; Quoted excluded tag: -#"tag with spaces"
              (let ((start (1+ pos))
                    (end (string-match "\"" s (1+ pos))))
                (if end
                    (progn
                      (push (substring s start end) excluded-tags)
                      (setq pos (1+ end)))
                  ;; No closing quote, treat as regular text
                  (push (substring s (- pos 2) (1+ pos)) texts)
                  (setq pos (1+ pos))))
            ;; Regular excluded tag: -#tag
            (let ((start pos)
                  (end pos))
              (while (and (< end len) 
                         (not (memq (aref s end) '(?\s ?\t))))
                (setq end (1+ end)))
              (when (> end start)
                (push (substring s start end) excluded-tags))
              (setq pos end))))
         
         ;; Handle tags (#tag or #"quoted tag")
         ((and (< pos len) (= (aref s pos) ?#))
          (setq pos (1+ pos))
          (if (and (< pos len) (= (aref s pos) ?\"))
              ;; Quoted tag: #"tag with spaces"
              (let ((start (1+ pos))
                    (end (string-match "\"" s (1+ pos))))
                (if end
                    (progn
                      (push (substring s start end) tags)
                      (setq pos (1+ end)))
                  ;; No closing quote, treat as regular text
                  (push (substring s (1- pos) (1+ pos)) texts)
                  (setq pos (1+ pos))))
            ;; Regular tag: #tag
            (let ((start pos)
                  (end pos))
              (while (and (< end len) 
                         (not (memq (aref s end) '(?\s ?\t)))
                         (string-match-p "[a-zA-Z0-9_+-.]" (char-to-string (aref s end))))
                (setq end (1+ end)))
              (when (> end start)
                (push (substring s start end) tags))
              (setq pos end))))
         
         ;; Handle folders [folder name]
         ((and (< pos len) (= (aref s pos) ?\[))
          (let ((end (string-match "]" s pos)))
            (if end
                (progn
                  (push (substring s (1+ pos) end) folders)
                  (setq pos (1+ end)))
              ;; No closing bracket, treat as text
              (let ((start pos)
                    (end pos))
                (while (and (< end len) 
                           (not (memq (aref s end) '(?\s ?\t))))
                  (setq end (1+ end)))
                (push (substring s start end) texts)
                (setq pos end)))))
         
         ;; Regular text
         (t
          (let ((start pos)
                (end pos))
            (while (and (< end len) 
                       (not (memq (aref s end) '(?\s ?\t))))
              (setq end (1+ end)))
            (push (substring s start end) texts)
            (setq pos end))))))
    
    (list :tags (nreverse tags)
          :excluded-tags (nreverse excluded-tags)
          :folders (nreverse folders)
          :text (string-join (nreverse texts) " "))))

(defun raindrop--meaningful-search-input-p (parsed)
  "Check if PARSED search input has meaningful content for searching."
  (let ((text (plist-get parsed :text))
        (tags (plist-get parsed :tags))
        (excluded-tags (plist-get parsed :excluded-tags))
        (folders (plist-get parsed :folders)))
    (or (and text (>= (length text) 2))
        (and (listp tags) (> (length tags) 0))
        (and (listp excluded-tags) (> (length excluded-tags) 0))
        (and (listp folders) (> (length folders) 0)))))

(defun raindrop--compose-search-string (text tags &optional excluded-tags)
  "Compose a search string from TEXT, TAGS, and EXCLUDED-TAGS for Raindrop API."
  (string-join
   (delq nil
         (list (and (listp tags) tags (raindrop--build-tag-search tags 'all))
               (and (listp excluded-tags) excluded-tags (raindrop--build-excluded-tag-search excluded-tags))
               (and text (not (string-empty-p text)) text)))
   " "))

(defun raindrop--build-search-endpoint-and-query (collection-id search page &optional limit)
  "Build endpoint and query params for search request.
Returns cons (ENDPOINT . QUERY-PARAMS)."
  (let* ((cid (if (numberp collection-id) collection-id raindrop-default-collection))
         (pp (max 1 (min (or limit 50) 100)))
         (pg (max 0 (or page 0)))
         (endpoint (format "/raindrops/%d" cid))
         (q (seq-filter #'consp
                        (append
                         (when (and search (not (string-empty-p search)))
                           (list (cons 'search search)))
                         (list (cons 'perpage pp)
                               (cons 'page pg)
                               (cons 'expand "collection"))))))
    (cons endpoint q)))

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

(defun raindrop--quote-tag-name (tag)
  "Quote TAG name for API search without # prefix."
  (let ((s (if (symbolp tag) (symbol-name tag) tag)))
    (if (string-match-p "[\t\n\r ]" s)
        (concat "\"" (replace-regexp-in-string "\"" "\\\"" s) "\"")
      s)))

(defun raindrop--build-excluded-tag-search (excluded-tags)
  "Build a search string for EXCLUDED-TAGS using NOT operators.
According to Raindrop API, you can exclude tags using '-tag'."
  (when (and excluded-tags (listp excluded-tags))
    (string-join
     (mapcar (lambda (tag) (format "-%s" (raindrop--quote-tag tag)))
             excluded-tags)
     " ")))

;;;; Normalization

(defun raindrop--normalize-item (raw)
  "Normalize RAW Raindrop item alist into a simpler alist."
  (let* ((link (alist-get 'link raw))
         (title (or (alist-get 'title raw) link))
         (excerpt (or (alist-get 'excerpt raw) ""))
         (tags-raw (alist-get 'tags raw))
         (tags (if (vectorp tags-raw) (append tags-raw nil) tags-raw))
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
  (let ((item-list (if (and items (listp items)) items '())))
    (raindrop--debug "normalize-items input count=%S" (length item-list))
    (let ((result (mapcar #'raindrop--normalize-item item-list)))
      (raindrop--debug "normalize-items output count=%S" (length result))
      result)))

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
:tags (list or string), :excluded-tags (list or string), :folders (list/string) or :folder (string),
:search (string), :collection (number or 0 for all), :limit (int), :sort (symbol), :match ('all|'any).
Returns list of normalized items."
  (when raindrop-debug
    (raindrop--debug "fetch %S" plist))
  (let* ((tags (plist-get plist :tags))
         (excluded-tags (plist-get plist :excluded-tags))
         (folders (or (plist-get plist :folders)
                      (let ((f (plist-get plist :folder))) (and f (list f)))))
         (search-text (plist-get plist :search))
         (match (or (plist-get plist :match) 'all))
         (limit (or (plist-get plist :limit) raindrop-default-limit))
         (input (string-join 
                 (delq nil
                   (append 
                    (when tags 
                      (list (raindrop--build-tag-search tags match)))
                    (when excluded-tags
                      (list (raindrop--build-excluded-tag-search excluded-tags)))
                    (when folders
                      (list (string-join (mapcar (lambda (folder) (concat "[" folder "]")) folders) " ")))
                    (when (and search-text (not (string-empty-p (string-trim search-text))))
                      (list (string-trim search-text)))))
                 " ")))
    (when raindrop-debug
      (raindrop--debug "fetch input=%S limit=%S" input limit))
    (if (string-empty-p (string-trim input))
        '()  ; Return empty list if no search criteria
      (progn
        (raindrop--debug "calling raindrop-search-bookmarks with input=%S limit=%S" input limit)
        (raindrop-search-bookmarks input nil limit)))))

(defun raindrop-fetch-async (plist callback)
  "Asynchronously fetch raindrops according to PLIST and call CALLBACK with (items err)."
  (let* ((tags (plist-get plist :tags))
         (excluded-tags (plist-get plist :excluded-tags))
         (folders (or (plist-get plist :folders)
                      (let ((f (plist-get plist :folder))) (and f (list f)))))
         (search-text (plist-get plist :search))
         (match (or (plist-get plist :match) 'all))
         (limit (or (plist-get plist :limit) raindrop-default-limit))
         (input (string-join 
                 (delq nil
                   (append 
                    (when tags 
                      (list (raindrop--build-tag-search tags match)))
                    (when excluded-tags
                      (list (raindrop--build-excluded-tag-search excluded-tags)))
                    (when folders
                      (list (string-join (mapcar (lambda (folder) (concat "[" folder "]")) folders) " ")))
                    (when (and search-text (not (string-empty-p (string-trim search-text))))
                      (list (string-trim search-text)))))
                 " ")))
    (when raindrop-debug
      (raindrop--debug "fetch-async input=%S limit=%S" input limit))
    (if (string-empty-p (string-trim input))
        (funcall callback '() nil)  ; Return empty list if no search criteria
      (raindrop-search-bookmarks input callback limit))))

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

;;;; Enhanced collection management

(defvar raindrop--collections-by-title nil
  "Hash table mapping collection titles (lowercase) to IDs.")

(defvar raindrop--collections-by-id nil
  "Hash table mapping collection IDs to titles.")

(defvar raindrop--collections-loading nil
  "Flag indicating if collections are currently being loaded.")

(defvar raindrop--collections-ready nil
  "Flag indicating if collections have been loaded and indexed.")

(defun raindrop--kv (item key)
  "Get value from ITEM (alist/plist) using KEY (keyword or symbol)."
  (let* ((k1 key)
         (k2 (if (keywordp key) (intern (substring (symbol-name key) 1))
               (intern (format ":%s" key)))))
    (or (plist-get item k1)
        (plist-get item k2)
        (and (consp item) (alist-get k1 item))
        (and (consp item) (alist-get k2 item)))))

(defun raindrop--build-collections-index (items)
  "Build index hash tables from collections ITEMS."
  (let ((by-title (make-hash-table :test 'equal))
        (by-id (make-hash-table :test 'eql)))
    (dolist (c items)
      (let* ((id (or (raindrop--kv c '_id)
                     (raindrop--kv c :_id)
                     (raindrop--kv c 'id)
                     (raindrop--kv c :id)))
             (title (or (raindrop--kv c 'title)
                        (raindrop--kv c :title)
                        (raindrop--kv c 'name)
                        (raindrop--kv c :name))))
        (when (and id title)
          (puthash (downcase title) id by-title)
          (puthash id title by-id))))
    (setq raindrop--collections-by-title by-title
          raindrop--collections-by-id by-id)))

(defun raindrop--ensure-collections-async (&optional callback)
  "Ensure collections are loaded, calling CALLBACK when ready."
  (unless (or raindrop--collections-ready
              raindrop--collections-loading)
    (setq raindrop--collections-loading t)
    (when raindrop-debug
      (raindrop--debug "loading collections…"))
    (raindrop-api-request-async
     "/collections" 'GET nil nil
     (lambda (res err)
       (setq raindrop--collections-loading nil)
       (if err
           (when raindrop-debug
             (raindrop--debug "collections load error: %s" err))
         (let ((items (alist-get 'items res)))
           (when (vectorp items)
             (setq items (append items nil)))
           (raindrop--build-collections-index (or items '()))))
       (setq raindrop--collections-ready t)
       (when callback (funcall callback))))))

(defun raindrop--collection-id-by-title (name)
  "Get collection ID by title NAME."
  (when (and name raindrop--collections-by-title)
    (gethash (downcase name) raindrop--collections-by-title)))

(defun raindrop--collection-title-by-id (cid)
  "Get collection title by ID CID."
  (cond
   ((eq cid -1) "Unsorted")
   ((hash-table-p raindrop--collections-by-id)
    (gethash cid raindrop--collections-by-id))
   (t nil)))

;;;; High-level search API

(defun raindrop-search-bookmarks (input &optional callback limit page)
  "Search Raindrop bookmarks with INPUT string.
If CALLBACK is provided, performs async search, otherwise sync.
Returns normalized items list (sync) or calls CALLBACK with (items err)."
  (let* ((parsed (raindrop--parse-search-input input))
         (tags (plist-get parsed :tags))
         (excluded-tags (plist-get parsed :excluded-tags))
         (folders (plist-get parsed :folders))
         (text (plist-get parsed :text))
         (folder-name (car (last folders)))
         (coll-id (or (and folder-name
                           (raindrop--collection-id-by-title folder-name))
                      raindrop-default-collection))
         (search (raindrop--compose-search-string text tags excluded-tags))
         (pair (raindrop--build-search-endpoint-and-query coll-id search page limit))
         (endpoint (car pair))
         (query (cdr pair)))
    (when raindrop-debug
      (raindrop--debug "search=%S endpoint=%s" (if (string-empty-p search) nil search) endpoint))
  (raindrop--debug "parsed input - tags=%S excluded-tags=%S folders=%S text=%S" tags excluded-tags folders text)
  (raindrop--debug "search string=%S endpoint=%s" search endpoint)
    (if callback
        (raindrop-api-request-async
         endpoint 'GET query nil
         (lambda (res err)
           (when (and err (string-match-p "HTTP 404\\|Network error" err))
             (setq res '((items))))
           (if err
               (funcall callback nil err)
             (let* ((raw (or (alist-get 'items res) '()))
                    (items (if (vectorp raw) (append raw nil) raw))
                    (norm (mapcar (lambda (x) (condition-case nil
                                                  (raindrop--normalize-item x)
                                                (error x)))
                                  items)))
               (funcall callback norm nil)))))
      (let* ((payload (raindrop-api-request endpoint 'GET query nil))
             (items-raw (alist-get 'items payload))
             (items (if (vectorp items-raw) (append items-raw nil) items-raw)))
        (raindrop--debug "API response - payload keys=%S items count=%S (vectorp=%S)" 
                 (mapcar #'car payload) (length items) (vectorp items-raw))
        (raindrop--debug "first raw item=%S" (car items))
        (let ((normalized (raindrop--normalize-items items)))
          (raindrop--debug "normalized count=%S first normalized=%S" 
                   (length normalized) (car normalized))
          normalized)))))

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
    (raindrop--debug "collections cache cleared")))

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
