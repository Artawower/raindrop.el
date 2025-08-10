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
;; - Fetch bookmarks by tags/collection with basic pagination.
;; - Shared utilities for renderers and org integrations.

;;; Code:

(require 'url)
(require 'url-http)
(require 'url-cache)
(require 'auth-source)
(require 'json)
(require 'seq)

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
  "Default maximum number of items to fetch."
  :type 'integer
  :group 'raindrop)

(defcustom raindrop-default-collection 0
  "Default collection ID used for queries.
По спецификации Raindrop: collectionId=0 означает «все коллекции». Для работы
по умолчанию используем 0 как «все». Укажите конкретный числовой ID для выборки
по отдельной коллекции."
  :type 'integer
  :group 'raindrop)

(defcustom raindrop-debug-enable nil
  "Включить подробные сообщения отладки HTTP-запросов."
  :type 'boolean
  :group 'raindrop)

(defconst raindrop--user-agent "raindrop.el/0.1.0 (+https://github.com/artawower/raindrop.el)"
  "User-Agent header for Raindrop requests.")

(defun raindrop--mask (s)
  "Return masked version of secret string S for logs."
  (when (stringp s)
    (let ((n (length s)))
      (cond
       ((<= n 6) "******")
       (t (concat (substring s 0 3) (make-string (- n 6) ?*) (substring s (- n 3))))))))

(defun raindrop--get-token-from-auth-source ()
  "Retrieve token from auth-source using configured hosts.
Tries each host from `raindrop-auth-source-hosts`, then falls back to
`raindrop-auth-source-host` for compatibility."
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

;; Tags parsing utilities
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
                (mapcar (lambda (t) (if (symbolp t) (symbol-name t) t)) tags)))
   ((stringp tags)
    (let* ((s tags)
           (has-comma (string-match-p "," s))
           (rx "\"\([^\"]+\)\"\|[^,\t\n\r ]+")
           (start 0)
           (out '()))
      (if has-comma
          (progn
            (while (string-match rx s start)
              (let* ((m (match-string 0 s))
                     (q (match-string 1 s))
                     (tok (or q m)))
                (push (string-trim tok ",\s+" ",\s+") out))
              (setq start (match-end 0)))
            (nreverse out))
        ;; No commas: split on whitespace, but keep quoted groups
        (let* ((rx2 "\"\([^\"]+\)\"\|[^\t\n\r ]+")
               (start2 0))
          (while (string-match rx2 s start2)
            (let ((q (match-string 1 s))
                  (m (match-string 0 s)))
              (push (or q m) out))
            (setq start2 (match-end 0)))
          (nreverse out)))))
   (t (list (format "%s" tags)))))

(defun raindrop--build-query (params)
  "Build URL query string from PARAMS alist ((key . value) ...)."
  (mapconcat (lambda (kv)
               (format "%s=%s"
                       (url-hexify-string (format "%s" (car kv)))
                       (url-hexify-string (format "%s" (cdr kv)))))
             params "&"))

(defun raindrop--url (endpoint &optional query-alist)
  "Compose full URL for ENDPOINT with QUERY-ALIST."
  (concat (replace-regexp-in-string "/$" "" raindrop-api-base)
          (if (string-prefix-p "/" endpoint) endpoint (concat "/" endpoint))
          (when query-alist
            (concat "?" (raindrop--build-query query-alist)))))

(defun raindrop--collection-endpoint (collection)
  "Вернуть базовый эндпоинт для чтения закладок по COLLECTION.
Для списка по коллекции — `/raindrops/<id>`; для «все коллекции» — `/raindrops/0`.

Примечание: глобальный поиск по всем коллекциям лучше выполнять через `/raindrops`
без ID. Используйте `raindrop--endpoint-for` для выбора с учётом search."
  (let* ((id (cond
              ((numberp collection) collection)
              ((or (eq collection 'all) (eq collection :all) (null collection)) raindrop-default-collection)
              (t raindrop-default-collection))))
    (let ((cid (if (<= id 0) 0 id)))
      (format "/raindrops/%s" cid))))

(defun raindrop--endpoint-for (collection search-present)
  "Выбрать корректный эндпоинт с учётом COLLECTION и наличия поиска.
Если `search-present` и COLLECTION указывает на «все коллекции» (<= 0),
используем глобальный `/raindrops` (без ID), иначе — `/raindrops/<id>`."
  (let* ((id (cond
              ((numberp collection) collection)
              ((or (eq collection 'all) (eq collection :all) (null collection)) raindrop-default-collection)
              (t raindrop-default-collection))))
    (if (and search-present (<= id 0))
        "/raindrops"
      (format "/raindrops/%s" (if (<= id 0) 0 id)))))

(defun raindrop-api-request (endpoint &optional method query-alist data)
  "Perform HTTP request to Raindrop API.
ENDPOINT like /raindrops/0. METHOD is a symbol 'GET (default), 'POST, etc.
QUERY-ALIST is appended to URL. DATA (string) is sent as body for non-GET.
Returns parsed JSON as alist. Signals user-error on HTTP or parse errors."
  (let* ((url-request-method (upcase (symbol-name (or method 'GET))))
         (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " (raindrop--get-token)))
                                      ("Content-Type" . "application/json")
                                      ("User-Agent" . ,raindrop--user-agent)))
         (url-request-data (and (not (equal url-request-method "GET")) data))
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
          (goto-char (point-min))
          (let ((status-code (or (and (boundp 'url-http-response-status)
                                      url-http-response-status)
                                 (progn
                                   (when (re-search-forward "^HTTP/[^ ]+ \([0-9]+\)" nil t)
                                     (string-to-number (match-string 1)))))))
            (when raindrop-debug-enable
              (message "raindrop.el: [sync] HTTP %s for %s" status-code endpoint))
            ;; Move to body start
            (re-search-forward "^$" nil 'move)
            ;; If non-2xx, try to parse error message body to aid diagnostics
            (unless (and status-code (<= 200 status-code) (< status-code 300))
              (let (body err-text)
                (setq body (buffer-substring-no-properties (point) (point-max)))
                (condition-case _
                    (let* ((json-object-type 'alist)
                           (json-array-type 'list)
                           (json-key-type 'symbol)
                           (obj (ignore-errors (json-parse-string body :object-type 'alist :array-type 'list :null-object nil :false-object :json-false)))
                           (msg (or (alist-get 'message obj)
                                    (alist-get 'error obj)
                                    (alist-get 'errorMessage obj))))
                      (setq err-text (and msg (format "%s" msg))))
                  (error (setq err-text nil)))
                (user-error "raindrop.el: HTTP %s for %s%s"
                            status-code endpoint
                            (if err-text (format ": %s" err-text) ""))))
            (let ((json-object-type 'alist)
                  (json-array-type 'list)
                  (json-key-type 'symbol))
              (condition-case err
                  (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil :false-object :json-false)
                (error (user-error "raindrop.el: JSON parse error: %s" (cadr err)))))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(defun raindrop-api-request-async (endpoint &optional method query-alist data callback)
  "Asynchronous HTTP request to Raindrop API.
CALLBACK is called as (func RESULT ERR), where only one of RESULT/ERR is non-nil."
  (let* ((url-request-method (upcase (symbol-name (or method 'GET))))
         (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " (raindrop--get-token)))
                                      ("Content-Type" . "application/json")
                                      ("User-Agent" . ,raindrop--user-agent)))
         (url-request-data (and (not (equal url-request-method "GET")) data))
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
                     (goto-char (point-min))
                     (let ((status-code (or (and (boundp 'url-http-response-status)
                                                 url-http-response-status)
                                            (progn
                                              (when (re-search-forward "^HTTP/[^ ]+ \([0-9]+\)" nil t)
                                                (string-to-number (match-string 1)))))))
                       (when raindrop-debug-enable
                         (message "raindrop.el: [async] %s %s -> HTTP %s" url-request-method full-url status-code))
                       (if (not (and status-code (<= 200 status-code) (< status-code 300)))
                           (setq err (format "HTTP %s for %s" status-code endpoint))
                         (re-search-forward "^$" nil 'move)
                         (condition-case e
                             (let ((json-object-type 'alist)
                                   (json-array-type 'list)
                                   (json-key-type 'symbol))
                               (setq result (json-parse-buffer :object-type 'alist :array-type 'list :null-object nil :false-object :json-false)))
                           (error (setq err (format "JSON parse error: %s" (cadr e)))))))))
                 (when (buffer-live-p (current-buffer)) (kill-buffer (current-buffer))))
             (funcall cb result err)))
         nil t t)))))

(defun raindrop--quote-tag (t)
  "Преобразовать имя тега T в токен поиска Raindrop: #tag или #\"multi word\"."
  (let ((s (if (symbolp t) (symbol-name t) t)))
    (if (string-match-p "[\t\n\r ]" s)
        (concat "#\"" (replace-regexp-in-string "\"" "\\\"" s) "\"")
      (concat "#" s))))

(defun raindrop--build-tag-search (tags match)
  "Собрать строку поиска для TAGS с учётом MATCH ('all|'any)."
  (let* ((ts (seq-filter (lambda (s) (and (stringp s) (> (length s) 0)))
                         (mapcar (lambda (t) (if (symbolp t) (symbol-name t) t)) tags)))
         (terms (mapcar #'raindrop--quote-tag ts)))
    (pcase match
      ('any (string-join (cons "match:OR" terms) " "))
      (_    (string-join terms " ")))))

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
          (cons :collection collection))) )

(defun raindrop-fetch (&rest plist)
  "Fetch raindrops according to PLIST keys:
:tags (list of strings/symbols), :collection (number or 0 for all),
:limit (int), :sort (symbol), :match ('all|'any). Returns list of normalized items."
  (message "fetch params: %s" plist)
  (let* ((tags (plist-get plist :tags))
         (collection (or (plist-get plist :collection) raindrop-default-collection))
         (limit (or (plist-get plist :limit) raindrop-default-limit))
         (match (or (plist-get plist :match) 'all))
         (search (and tags (raindrop--build-tag-search (if (listp tags) tags (list tags)) match)))
         (endpoint (raindrop--endpoint-for collection (and search t)))
         ;; perpage: up to 100. We’ll loop pages if needed later; MVP fetches one page.
         (query `((perpage . ,(min limit 100))
                  ,@(when search `((search . ,search)))))
         payload items coll-id alt-endpoint alt-payload alt-items)
    (condition-case err
        (progn
          (setq payload (raindrop-api-request endpoint 'GET query nil)
                items (alist-get 'items payload)
                coll-id (cond
                         ((numberp collection) collection)
                         ((or (eq collection 'all) (eq collection :all) (null collection)) raindrop-default-collection)
                         (t raindrop-default-collection)))
          ;; Fallback: empty items on one endpoint, try the alternate endpoint
          (when (and (<= coll-id 0) search (or (null items) (equal items '())))
            (setq alt-endpoint (if (string= endpoint "/raindrops") "/raindrops/0" "/raindrops")
                  alt-payload (raindrop-api-request alt-endpoint 'GET query nil)
                  alt-items (alist-get 'items alt-payload)
                  items alt-items))
          (mapcar #'raindrop--normalize-item (if (and items (listp items)) items '())))
      (user-error
       (let* ((msg (error-message-string err))
              (coll-id (cond
                        ((numberp collection) collection)
                        ((or (eq collection 'all) (eq collection :all) (null collection)) raindrop-default-collection)
                        (t raindrop-default-collection))))
         (if (and (string-match-p "404" msg)
                  (<= coll-id 0)
                  search)
             (let* ((alt-endpoint (if (string= endpoint "/raindrops") "/raindrops/0" "/raindrops"))
                    (alt-payload (raindrop-api-request alt-endpoint 'GET query nil))
                    (alt-items (alist-get 'items alt-payload)))
               (mapcar #'raindrop--normalize-item (if (and alt-items (listp alt-items)) alt-items '())))
           (signal (car err) (cdr err))))))))

(defun raindrop-fetch-async (plist callback)
  "Asynchronously fetch raindrops according to PLIST and call CALLBACK with (items err)."
  (let* ((tags (plist-get plist :tags))
         (collection (or (plist-get plist :collection) raindrop-default-collection))
         (limit (or (plist-get plist :limit) raindrop-default-limit))
         (match (or (plist-get plist :match) 'all))
         (search (and tags (raindrop--build-tag-search (if (listp tags) tags (list tags)) match)))
         (endpoint (raindrop--endpoint-for collection (and search t)))
         (query `((perpage . ,(min limit 100))
                  ,@(when search `((search . ,search))))))
    (let ((coll-id (cond
                    ((numberp collection) collection)
                    ((or (eq collection 'all) (eq collection :all) (null collection)) raindrop-default-collection)
                    (t raindrop-default-collection))))
      (raindrop-api-request-async
       endpoint 'GET query nil
       (lambda (payload err)
         (if err
             (let ((msg (format "%s" err)))
               (if (and (string-match-p "404" msg) (<= coll-id 0) search)
                   (let ((alt-endpoint (if (string= endpoint "/raindrops") "/raindrops/0" "/raindrops")))
                     (raindrop-api-request-async
                      alt-endpoint 'GET query nil
                      (lambda (payload2 err2)
                        (if err2
                            (funcall callback nil err2)
                          (let ((items2 (alist-get 'items payload2)))
                            (funcall callback (mapcar #'raindrop--normalize-item (if (and items2 (listp items2)) items2 '())) nil))))))
                 (funcall callback nil err)))
           (let ((items (alist-get 'items payload)))
             (if (and (<= coll-id 0) search (or (null items) (equal items '())))
                 (let ((alt-endpoint (if (string= endpoint "/raindrops") "/raindrops/0" "/raindrops")))
                   (raindrop-api-request-async
                    alt-endpoint 'GET query nil
                    (lambda (payload3 err3)
                      (if err3
                          (funcall callback nil err3)
                        (let ((items3 (alist-get 'items payload3)))
                          (funcall callback (mapcar #'raindrop--normalize-item (if (and items3 (listp items3)) items3 '())) nil))))))
               (funcall callback (mapcar #'raindrop--normalize-item (if (and items (listp items)) items '())) nil)))))))))

;;; Filters API

(defun raindrop--filters-endpoint (collection)
  "Вернуть эндпоинт Filters для COLLECTION.
Документация Raindrop: GET /filters/{collectionId}, где collectionId = 0 — все коллекции.
В нашем API convention значение < 0 трактуется как «все коллекции», поэтому
тут оно маппится на 0."
  (let* ((id (cond
              ((numberp collection) collection)
              ((or (eq collection 'all) (eq collection :all) (null collection)) raindrop-default-collection)
              (t raindrop-default-collection)))
         (cid (if (<= id 0) 0 id)))
    (format "/filters/%s" cid)))

(defun raindrop-filters (&rest plist)
  "Получить агрегированные фильтры для заданной коллекции.
PLIST ключи:
  :collection — id коллекции (по умолчанию `raindrop-default-collection` → «все»)
  :search — строка поиска (как в /raindrops search=)
  :tags-sort — 'count (по умолчанию) или '_id

Возвращает alist согласно спецификации: поля broken, duplicates, important,
notag, tags (список {_id count}), types (список {_id count})."
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
  "Сделать сырой запрос к Filters API для COLLECTION с SEARCH.
COLLECTION по умолчанию — «все» (`raindrop-default-collection`)."
  (let* ((coll (or collection raindrop-default-collection))
         (payload (raindrop-filters :collection coll :search search)))
    payload))

;;; Collections helpers (to discover valid IDs)

(defun raindrop-collections ()
  "Получить список коллекций пользователя (alist payload).
Полезно, чтобы узнать корректные ID коллекций для запросов /raindrops/{id}."
  (raindrop-api-request "/collections" 'GET nil nil))

;;;###autoload
(defun raindrop-debug-collections ()
  "Вернуть сырой payload со списком коллекций."
  (raindrop-collections))

;;; Minimal debug helpers

;;;###autoload
(defun raindrop-debug-search (tags &optional collection)
  "Сделать сырой запрос к API с фильтром по TAGS и вернуть payload (alist).
TAGS — строка или список тегов. COLLECTION — id коллекции (по умолчанию 0 — все коллекции)."
  (let* ((tags-list (raindrop-parse-tags tags))
         (search (and tags-list (raindrop--build-tag-search tags-list 'all)))
         (coll (if (numberp collection) collection 0))
         (endpoint (raindrop--endpoint-for coll (and search t)))
         (payload (raindrop-api-request endpoint 'GET `((perpage . 5) ,@(when search `((search . ,search)))) nil)))
    payload))

;;;###autoload
(defun raindrop-debug-fetch (tags &optional collection limit)
  "Получить нормализованный список закладок по TAGS.
COLLECTION — id коллекции (по умолчанию 0 — все). LIMIT — максимум элементов (по умолчанию 10)."
  (let* ((tags-list (raindrop-parse-tags tags))
         (coll (if (numberp collection) collection 0))
         (lim (or limit 10)))
    (apply #'raindrop-fetch (list :tags tags-list :collection coll :limit lim :match 'all))))

(provide 'raindrop)

;;; raindrop.el ends here
