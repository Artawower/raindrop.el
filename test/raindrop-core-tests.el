;;; raindrop-core-tests.el --- ERT tests for raindrop.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'raindrop)
(require 'raindrop-org)

;; Backward compatibility for tests

;; Mock async API request function for tests
(defun raindrop-api-request-async (endpoint method query data callback)
  "Mock async API request function for tests."
  ;; Default mock: return empty result with no error
  (funcall callback nil nil))

;; Mock async fetch function for tests with compatibility wrapper
(defun raindrop-fetch-async (input callback &optional limit page)
  "Compatibility wrapper for tests - handles both string and plist input."
  (let* ((search-input 
          (cond
           ;; If input is a plist, convert to search string
           ((and (listp input) (plist-get input :tags))
            (let* ((tags (plist-get input :tags))
                   (folders (plist-get input :folders))
                   (tag-strings (mapcar (lambda (tag) (format "#%s" tag)) tags))
                   (folder-strings (mapcar (lambda (folder) (format "[%s]" folder)) folders)))
              (string-join (append tag-strings folder-strings) " ")))
           ;; If input is already a string, use as-is
           ((stringp input) input)
           ;; Fallback
           (t "")))
         (actual-limit (or limit (plist-get input :limit) 50)))
    (raindrop-search-bookmarks search-input callback actual-limit page)))

(ert-deftest raindrop-parse-tags-nil ()
  (should (equal (raindrop-parse-tags nil) nil)))

(ert-deftest raindrop-parse-tags-list-mixed ()
  (should (equal (raindrop-parse-tags '(foo "bar")) '("foo" "bar"))))

(ert-deftest raindrop-parse-tags-string-commas-and-quotes ()
  (should (equal (raindrop-parse-tags "foo, \"multi word\",bar")
                 '("foo" "multi word" "bar"))))

(ert-deftest raindrop-parse-tags-string-spaces-no-commas ()
  (should (equal (raindrop-parse-tags "foo bar baz")
                 '("foo" "bar" "baz"))))

(ert-deftest raindrop-quote-tag-basic-and-spaces ()
  (should (equal (raindrop--quote-tag 'foo) "#foo"))
  (should (equal (raindrop--quote-tag "multi word") "#\"multi word\"")))

(ert-deftest raindrop-build-tag-search-all-vs-any ()
  (let ((tags '("foo" "multi word" "bar")))
    (should (equal (raindrop--build-tag-search tags 'all)
                   "#foo #\"multi word\" #bar"))
    (should (equal (raindrop--build-tag-search tags 'any)
                   (string-join (list "match:OR" "#foo" "#\"multi word\"" "#bar") " ")))))

(ert-deftest raindrop-endpoint-for-selection ()
  (should (equal (raindrop--endpoint-for 0 nil) "/raindrops/0"))
  (should (equal (raindrop--endpoint-for 0 t) "/raindrops"))
  (should (equal (raindrop--endpoint-for 5 t) "/raindrops/5"))
  (should (equal (raindrop--endpoint-for 5 nil) "/raindrops/5")))

(ert-deftest raindrop-filters-endpoint-mapping ()
  (should (equal (raindrop--filters-endpoint 0) "/filters/0"))
  (should (equal (raindrop--filters-endpoint -1) "/filters/0"))
  (should (equal (raindrop--filters-endpoint 7) "/filters/7")))

(ert-deftest raindrop-render-org-list-basic ()
  (let ((raindrop-org-excerpt-next-line t))
    (let* ((items (list (list :link "https://ex.com" :title "Title" :excerpt "Desc")))
           (out (raindrop-render-org-list items)))
      (should (string-match-p "^- \\[\\[https://ex\\.com\\]\\[Title\\]\\]" out))
      (should (string-match-p "\n   Desc$" out)))))

(ert-deftest raindrop-mask-secret ()
  (should (equal (raindrop--mask "short") "******"))
  (should (equal (raindrop--mask "abcdefghijkl") "abc******jkl")))

(ert-deftest raindrop-build-query ()
  (let ((s (raindrop--build-query '((a . 1) (b . 2)))))
    (should (or (string= s "a=1&b=2") (string= s "b=2&a=1")))))

(ert-deftest raindrop-normalize-item-fallbacks ()
  (let* ((raw '((link . "https://ex.com") (excerpt . "e") (_id . 1) (tags . (t1 t2))))
         (it (raindrop--normalize-item raw)))
    (should (equal (alist-get :title it) "https://ex.com"))
    (should (equal (alist-get :excerpt it) "e"))
    (should (equal (alist-get :id it) 1))))

(ert-deftest raindrop-get-token-precedence ()
  (let ((raindrop-token-source '(custom env auth-source))
        (raindrop-custom-token "CUST")
        (getenv (lambda (_k) "ENV")))
    (should (equal (raindrop--get-token) "CUST")))
  (cl-letf* (((symbol-function 'getenv) (lambda (_k) "ENV"))
             ((symbol-function 'auth-source-search)
              (lambda (&rest _)
                (list (list :secret (lambda () "AUTH")))))
             (raindrop-token-source '(env auth-source)))
    (should (equal (raindrop--get-token) "ENV"))))

(ert-deftest raindrop-api-request-success-and-error ()
  (cl-letf (((symbol-function 'request)
             (lambda (_url &rest args)
               (let ((success (plist-get args :success)))
                 (when success
                   (funcall success :data '((ok . t) (items . (1 2))))))))
            ((symbol-function 'raindrop--get-token) (lambda () "TOK")))
    (let ((res (raindrop-api-request "/x" 'GET nil nil)))
      (should (eq (alist-get 'ok res) t))
      (should (equal (alist-get 'items res) '(1 2)))))
  (cl-letf (((symbol-function 'request)
             (lambda (_url &rest args)
               (let ((error (plist-get args :error)))
                 (when error
                   (funcall error :error-thrown '(error . "HTTP 404"))))))
            ((symbol-function 'raindrop--get-token) (lambda () "TOK")))
    (should-error (raindrop-api-request "/x" 'GET nil nil) :type 'user-error)))

(ert-deftest raindrop-fetch-fallback-on-empty-items ()
  (let ((calls '()))
    (cl-letf (((symbol-function 'raindrop-api-request)
               (lambda (endpoint &rest _)
                 (push endpoint calls)
                 (cond
                  ((string= endpoint "/raindrops/0") '((items . (((_id . 1) (link . "l") (title . "t"))))))
                  (t '((items)))))))
      (let* ((items (apply #'raindrop-fetch (list :tags '("a") :collection 0 :limit 5 :match 'all))))
        (should (= (length items) 1))
        (should (member "/raindrops/0" calls))))))

(ert-deftest raindrop-debug-search-builds-search ()
  (cl-letf (((symbol-function 'raindrop-api-request)
             (lambda (endpoint method query &rest _)
               (list (cons 'endpoint endpoint)
                     (cons 'method method)
                     (cons 'query query)))))
    (let* ((payload (raindrop-debug-search '(foo "multi word") 0))
           (q (alist-get 'query payload)))
      (should (equal (alist-get 'perpage q) 5))
      (should (string-match-p "#foo" (alist-get 'search q)))
      (should (string-match-p "#\"multi word\"" (alist-get 'search q))))))

(ert-deftest raindrop-dblock-writer-renders-empty-and-items ()
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN: raindrop :tags foo :match all :limit 5\n#+END:\n")
    (goto-char (point-min))
    (let ((raindrop-org-excerpt-next-line t))
      (cl-letf (((symbol-function 'raindrop-fetch) (lambda (&rest _) nil)))
        (org-dblock-write:raindrop '(:tags "foo" :match all :limit 5))
        (goto-char (point-min))
        (should (search-forward "- No results" nil t)))
      (erase-buffer)
      (insert "#+BEGIN: raindrop :tags foo :match all :limit 5\n#+END:\n")
      (goto-char (point-min))
      (cl-letf (((symbol-function 'raindrop-fetch)
                 (lambda (&rest _)
                   (list (list :link "https://ex.com" :title "T" :excerpt "E")))))
        (org-dblock-write:raindrop '(:tags "foo" :match all :limit 5))
        (goto-char (point-min))
        (should (search-forward "[[https://ex.com][T]]" nil t))
        (should (search-forward "\n   E" nil t))))))

(ert-deftest raindrop-sanitize-org-trims-and-flattens ()
  (let ((s (concat "  Title\nwith\nnewlines  ")))
    (should (equal (raindrop-org--sanitize s) "Title with newlines")))
  (should (equal (raindrop-org--sanitize nil) nil)))

(ert-deftest raindrop-endpoint-for-nil-and-all ()
  (should (equal (raindrop--endpoint-for nil nil) "/raindrops/0"))
  (should (equal (raindrop--endpoint-for 'all nil) "/raindrops/0"))
  (should (equal (raindrop--endpoint-for :all t) "/raindrops")))

(ert-deftest raindrop-filters-builds-query ()
  (cl-letf (((symbol-function 'raindrop-api-request)
             (lambda (endpoint _m query _data)
               (list (cons 'endpoint endpoint) (cons 'query query)))))
    (let* ((res (raindrop-filters :collection 0 :search "#foo" :tags-sort '_id))
           (q (alist-get 'query res)))
      (should (equal (alist-get 'tagsSort q) "_id"))
      (should (equal (alist-get 'search q) "#foo")))))

(ert-deftest raindrop-fetch-perpage-limit ()
  (let (seen-query)
    (cl-letf (((symbol-function 'raindrop-api-request)
               (lambda (_e _m query _d)
                 (setq seen-query query)
                 '((items . ())))))
      (raindrop-fetch :tags '("a") :limit 20)
      (should (equal (alist-get 'perpage seen-query) 20))
      (raindrop-fetch :tags '("a") :limit 1000)
      (should (equal (alist-get 'perpage seen-query) 100)))))

(ert-deftest raindrop-fetch-async-perpage-limit ()
  (let (seen-query)
    (cl-letf (((symbol-function 'raindrop-api-request-async)
               (lambda (_e _m query _d cb)
                 (setq seen-query query)
                 (funcall cb '((items)) nil))))
      (raindrop-fetch-async (list :tags '("a") :limit 5) (lambda (_ _)))
      (should (equal (alist-get 'perpage seen-query) 5))
      (raindrop-fetch-async (list :tags '("a") :limit 200) (lambda (_ _)))
      (should (equal (alist-get 'perpage seen-query) 100)))))

(ert-deftest raindrop-token-errors-and-auth-source-secret-fn ()
  (let ((raindrop-token-source '()))
    (should-error (raindrop--get-token) :type 'user-error))
  (let ((raindrop-token-source '(custom))
        (raindrop-custom-token ""))
    (should-error (raindrop--get-token) :type 'user-error))
  (cl-letf (((symbol-function 'auth-source-search)
             (lambda (&rest _) (list (list :secret (lambda () "AUTH")))))
            (raindrop-token-source '(auth-source)))
    (should (equal (raindrop--get-token) "AUTH"))))

(ert-deftest raindrop-http-error-bodies-and-non-json ()
  "Test that API request handles HTTP errors correctly with request library."
  ;; Test HTTP 400 error
  (cl-letf (((symbol-function 'request)
             (lambda (_url &rest args)
               (let ((error (plist-get args :error)))
                 (when error
                   (funcall error :error-thrown '(error . "HTTP 400: Bad Request")
                            :response nil)))))
            ((symbol-function 'raindrop--get-token) (lambda () "TOK")))
    (should-error (raindrop-api-request "/x" 'GET nil nil) :type 'user-error))
  ;; Test HTTP 500 error  
  (cl-letf (((symbol-function 'request)
             (lambda (_url &rest args)
               (let ((error (plist-get args :error)))
                 (when error
                   (funcall error :error-thrown '(error . "HTTP 500: Server Error")
                            :response nil)))))
            ((symbol-function 'raindrop--get-token) (lambda () "TOK")))
    (should-error (raindrop-api-request "/x" 'GET nil nil) :type 'user-error)))

(ert-deftest raindrop-api-async-network-error-status ()
  "Test that async API request handles network errors correctly."
  (let (cb-result cb-err)
    (cl-letf (((symbol-function 'raindrop-api-request-async)
               (lambda (_endpoint _method _query _data callback)
                 (funcall callback nil "Network error: timeout")))
              ((symbol-function 'raindrop--get-token) (lambda () "TOK")))
      (raindrop-api-request-async "/x" 'GET nil nil (lambda (r e) (setq cb-result r cb-err e)))
      (should (null cb-result))
      (should (string-match-p "Network error" (or cb-err ""))))))

(ert-deftest raindrop-parse-tags-empty-string-and-spaces ()
  (should (equal (raindrop-parse-tags "") nil))
  (should (equal (raindrop-parse-tags "   \t\n") nil)))

(ert-deftest raindrop-dblock-reinvoke-updates-content ()
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN: raindrop :tags foo :match all :limit 5\n#+END:\n")
    (goto-char (point-min))
    (let ((raindrop-org-excerpt-next-line t))
      (cl-letf (((symbol-function 'raindrop-fetch)
                 (lambda (&rest _) (list (list :link "https://a" :title "A" :excerpt "1")))))
        (org-dblock-write:raindrop '(:tags "foo" :match all :limit 5)))
      (goto-char (point-min))
      (should (search-forward "[[https://a][A]]" nil t))
      (should (search-forward "\n   1" nil t))
      (goto-char (point-min))
      (cl-letf (((symbol-function 'raindrop-fetch)
                 (lambda (&rest _) (list (list :link "https://b" :title "B" :excerpt "2")))))
        (org-dblock-write:raindrop '(:tags "foo" :match all :limit 5)))
      (goto-char (point-min))
      (should (search-forward "[[https://b][B]]" nil t))
      (should (search-forward "\n   2" nil t))
      (should-not (search-forward "[[https://a][A]]" nil t)))))

(ert-deftest raindrop-api-request-async-success-and-error ()
  "Test that async API request handles success and error responses correctly."
  ;; Test success case
  (let (cb-result cb-err)
    (cl-letf (((symbol-function 'raindrop-api-request-async)
               (lambda (_endpoint _method _query _data callback)
                 (funcall callback '((ok . t)) nil))))
      (raindrop-api-request-async "/x" 'GET nil nil
                                  (lambda (res err) (setq cb-result res cb-err err)))
      (should (equal (alist-get 'ok cb-result) t))
      (should (null cb-err))))
  ;; Test error case  
  (let (cb-result cb-err)
    (cl-letf (((symbol-function 'raindrop-api-request-async)
               (lambda (_endpoint _method _query _data callback)
                 (funcall callback nil "HTTP 500: Server error"))))
      (raindrop-api-request-async "/x" 'GET nil nil
                                  (lambda (res err) (setq cb-result res cb-err err)))
      (should (null cb-result))
      (should (string-match-p "HTTP 500" (or cb-err ""))))))

(ert-deftest raindrop-fetch-async-fallback-on-empty-items ()
  (let ((calls '()))
    (cl-letf (((symbol-function 'raindrop-api-request-async)
               (lambda (endpoint _m _q _d cb)
                 (push endpoint calls)
                 (cond
                  ((string= endpoint "/raindrops/0") (funcall cb '((items . (((_id . 1) (link . \"l\") (title . \"t\"))))) nil))
                  (t (funcall cb '((items)) nil))))))
      (let (done items err)
        (raindrop-fetch-async (list :tags '("a") :collection 0 :limit 5 :match 'all)
                              (lambda (its e) (setq items its err e done t)))
        (should done)
        (should (= (length items) 1))
        (should (member "/raindrops/0" calls))))))

(ert-deftest raindrop-under-heading-errors-without-tags ()
  (with-temp-buffer
    (org-mode)
    (insert "* Heading without tags\n")
    (goto-char (point-min))
    (should-error (raindrop-insert-or-update-links-under-heading) :type 'user-error)))

(ert-deftest raindrop-under-heading-inserts-block-and-replaces ()
  "Test that raindrop dynamic block is inserted and populated under headings."
  (with-temp-buffer
    (org-mode)
    (insert "* Test :foo:bar:\nBody\n")
    (goto-char (point-min))
    (let ((raindrop-org-excerpt-next-line t))
      (cl-letf (((symbol-function 'raindrop-search-bookmarks)
                 (lambda (_input callback &optional _limit _page)
                   (funcall callback (list (list :link "https://ex.com" :title "T" :excerpt "E")) nil))))
        (raindrop-insert-or-update-links-under-heading)
        (goto-char (point-min))
        (should (search-forward "#+BEGIN: raindrop" nil t))
        (should (search-forward "[[https://ex.com][T]]" nil t))
        (should (search-forward "\n   E" nil t))))))

(ert-deftest raindrop-quote-folder-basic-and-spaces ()
  (should (equal (raindrop--quote-folder 'Work) "collection:Work"))
  (should (equal (raindrop--quote-folder "Multi Word") "collection:\"Multi Word\"")))

;; ========== Tests for new functionality ==========

(ert-deftest raindrop-search-spinner-functionality ()
  "Test spinner start, stop, and frame rotation."
  (require 'raindrop-search)
  
  ;; Test initial state
  (should-not raindrop-search--loading)
  (should (equal (raindrop-search--get-spinner-frame) ""))
  
  ;; Test starting spinner
  (raindrop-search--start-spinner)
  (should raindrop-search--loading)
  (should raindrop-search--spinner-timer)
  (should (member (raindrop-search--get-spinner-frame) raindrop-search-spinner-frames))
  
  ;; Test stopping spinner
  (raindrop-search--stop-spinner)
  (should-not raindrop-search--loading)
  (should-not raindrop-search--spinner-timer)
  (should (equal (raindrop-search--get-spinner-frame) "")))

(ert-deftest raindrop-search-spinner-frames-customization ()
  "Test that spinner frames can be customized."
  (require 'raindrop-search)
  
  (let ((raindrop-search-spinner-frames '("●" "○" "◐" "◑")))
    (raindrop-search--start-spinner)
    (should (member (raindrop-search--get-spinner-frame) '("●" "○" "◐" "◑")))
    (raindrop-search--stop-spinner)))

(ert-deftest raindrop-org-tag-rendering-improvements ()
  "Test improved tag rendering with vector and list handling."
  (require 'raindrop-org)
  
  ;; Test with list tags
  (let ((raindrop-org-render-tags t))
    (should (equal (raindrop-org--format-tags '("tag1" "tag2")) "  (tag1, tag2)"))
    (should (equal (raindrop-org--format-tags '("single")) "  (single)"))
    (should (equal (raindrop-org--format-tags nil) nil))
    (should (equal (raindrop-org--format-tags '()) nil)))
  
  ;; Test with vector tags (converted to lists)
  (let ((raindrop-org-render-tags t))
    (should (equal (raindrop-org--format-tags ["vec1" "vec2"]) "  (vec1, vec2)"))
    (should (equal (raindrop-org--format-tags ["single"]) "  (single)")))
  
  ;; Test with render disabled
  (let ((raindrop-org-render-tags nil))
    (should (equal (raindrop-org--format-tags '("tag1" "tag2")) nil))))

(ert-deftest raindrop-search-bookmarks-functionality ()
  "Test new search API function."
  ;; Test with direct API call (synchronous)
  (cl-letf (((symbol-function 'raindrop-api-request)
             (lambda (_endpoint _method _query _data)
               '((items . [((title . "Test Item") (link . "https://test.com") (tags . ["test"]))])))))
    (let ((result (raindrop-search-bookmarks "#test" nil)))
      (should (= (length result) 1))
      (should (equal (alist-get :title (car result)) "Test Item"))
      (should (listp (alist-get :tags (car result)))))))

(ert-deftest raindrop-vector-to-list-conversion ()
  "Test that API vectors are properly converted to lists."
  (let* ((raw-item '((title . "Test") (tags . ["tag1" "tag2"])))
         (normalized (raindrop--normalize-item raw-item))
         (tags (alist-get :tags normalized)))
    (should (listp tags))
    (should (equal tags '("tag1" "tag2")))))

(ert-deftest raindrop-search-ui-candidates-with-spinner ()
  "Test UI candidates display with spinner functionality."
  (require 'raindrop-search)
  
  ;; Test loading state shows spinner
  (let ((raindrop-search--loading t)
        (raindrop-search--spinner-index 0)
        (raindrop-search-spinner-frames '("|" "/" "-" "\\")))
    (let ((candidates (raindrop-search--ui-candidates)))
      (should (= (length candidates) 1))
      (should (string-match-p "| Loading..." (car candidates)))))
  
  ;; Test normal results when not loading
  (let ((raindrop-search--loading nil)
        (raindrop-search--items '(((:title . "Test") (:link . "https://test.com")))))
    (let ((candidates (raindrop-search--ui-candidates)))
      (should (> (length candidates) 0))
      (should-not (string-match-p "Loading" (car candidates))))))

(ert-deftest raindrop-search-input-parsing-improvements ()
  "Test improved search input parsing for new API."
  ;; Test tag parsing
  (let ((parsed (raindrop--parse-search-input "#tag1 #tag2 some text")))
    (should (equal (plist-get parsed :tags) '("tag1" "tag2")))
    (should (equal (plist-get parsed :text) "some text")))
  
  ;; Test folder parsing (single word)
  (let ((parsed (raindrop--parse-search-input "[Work] #tag")))
    (should (equal (plist-get parsed :folders) '("Work")))
    (should (equal (plist-get parsed :tags) '("tag"))))
  
  ;; Test mixed input
  (let ((parsed (raindrop--parse-search-input "#important [Archive] project notes")))
    (should (equal (plist-get parsed :tags) '("important")))
    (should (equal (plist-get parsed :folders) '("Archive")))
    (should (equal (plist-get parsed :text) "project notes"))))

(ert-deftest raindrop-build-search-folders-and-tags ()
  (let* ((s-all (raindrop--build-search '("t1") '("Folder A") 'all))
         (s-any (raindrop--build-search '("t1") '("Folder A") 'any)))
    (should (string-match-p "#t1" s-all))
    (should (string-match-p "collection:\\\"Folder A\\\"" s-all))
    (should (not (string-match-p "match:OR" s-all)))
    (should (string-match-p "^match:OR" s-any))
    (should (string-match-p "#t1" s-any))
    (should (string-match-p "collection:\\\"Folder A\\\"" s-any))))

(ert-deftest raindrop-dblock-accepts-folder-param ()
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN: raindrop :folder \"Folder A\" :match all :limit 5\n#+END:\n")
    (goto-char (point-min))
    (let (saw-folders)
      (cl-letf (((symbol-function 'raindrop-fetch)
                 (lambda (&rest plist)
                   (setq saw-folders (plist-get plist :folders))
                   nil)))
        (org-dblock-write:raindrop '(:folder "Folder A" :match all :limit 5))
        (should (equal saw-folders '("Folder A")))))))


;;; smart grouping tests

(ert-deftest raindrop-smart-eligible-tags-respects-min-and-stop ()
  (let* ((items (list
                 (list :tags '("ascii" "cli"))
                 (list :tags '("ascii" "cli"))
                 (list :tags '("file manager" "cli"))
                 (list :tags '("file manager"))
                 (list :tags '("unique"))))
         (raindrop-org-smart-min-count 2)
         (raindrop-org-smart-max-groups 8)
         (raindrop-org-smart-stop-tags '("cli"))
         (freqs (raindrop-org--tag-frequencies items))
         (selected (raindrop-org--eligible-tags freqs)))
    (should (member "ascii" selected))
    (should (member "file manager" selected))
    (should-not (member "cli" selected))
    (should-not (member "unique" selected))))

(ert-deftest raindrop-smart-max-groups-cap ()
  (let* ((items (mapcar (lambda (i)
                          ;; 20 тегов t1..t20, каждый встречается два раза
                          (list :tags (list (format "t%d" (1+ (/ i 2))))))
                        (number-sequence 0 39)))
         (raindrop-org-smart-min-count 2)
         (raindrop-org-smart-max-groups 5)
         (raindrop-org-smart-stop-tags '())
         (freqs (raindrop-org--tag-frequencies items))
         (selected (raindrop-org--eligible-tags freqs)))
    (should (= (length selected) 5))))

(ert-deftest raindrop-smart-group-items-auto-basic ()
  (let* ((items (list
                 (list :link "https://a" :title "Yazi"   :tags '("file manager" "cli" "tui"))
                 (list :link "https://b" :title "ncdu"   :tags '("file manager" "cli"))
                 (list :link "https://c" :title "toilet" :tags '("ascii" "cli"))
                 (list :link "https://d" :title "figlet" :tags '("ascii" "cli"))
                 (list :link "https://e" :title "misc"   :tags '("rare"))))
         (raindrop-org-smart-min-count 2)
         (raindrop-org-smart-max-groups 8)
         (raindrop-org-smart-stop-tags '("cli"))
         (groups (raindrop-org--group-items-auto items)))
    (let ((names (mapcar #'car groups)))
      (should (member "Ascii" names))
      (should (member "File manager" names))
      (should (member "Other" names)))
    (let* ((ascii (assoc "Ascii" groups))
           (fm    (assoc "File manager" groups))
           (other (assoc "Other" groups)))
      (should (= (length (cdr ascii)) 2))
      (should (= (length (cdr fm)) 2))
      (should (= (length (cdr other)) 1))
      (let* ((it (car (cdr other)))
             (lnk (or (plist-get it :link) (alist-get :link it))))
        (should (equal lnk "https://e"))))))

(ert-deftest raindrop-smart-render-grouped-respects-heading-level ()
  (let* ((items (list
                 (list :link "https://a" :title "A" :tags '("x" "cli"))
                 (list :link "https://b" :title "B" :tags '("x"))
                 (list :link "https://c" :title "C" :tags '("y"))))
         (raindrop-org-smart-min-count 2)
         (raindrop-org-smart-stop-tags '("cli"))
         (raindrop-org-smart-heading-level 2)
         (org (raindrop-org--render-grouped
               (raindrop-org--group-items-auto items))))
    (should (string-match-p "^\\*\\* X$" org))
    (should (string-match-p "^- \\[\\[https://a\\]\\[A\\]\\]" org))
    (should (string-match-p "^- \\[\\[https://b\\]\\[B\\]\\]" org))
    (should (string-match-p "^\\*\\* Other$" org))
    (should (string-match-p "\\[\\[https://c\\]\\[C\\]\\]" org))))

(ert-deftest raindrop-dblock-smart-flag-produces-headings ()
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN: raindrop :tags foo :match all :limit 5 :smart t\n#+END:\n")
    (goto-char (point-min))
    (let ((raindrop-org-smart-min-count 1)
          (raindrop-org-smart-stop-tags '())
          (raindrop-org-smart-heading-level 3))
      (cl-letf (((symbol-function 'raindrop-fetch)
                 (lambda (&rest _)
                   (list
                    (list :link "https://a" :title "A" :tags '("alpha"))
                    (list :link "https://b" :title "B" :tags '("beta"))))))
        (org-dblock-write:raindrop '(:tags "foo" :match all :limit 5 :smart t))
        (goto-char (point-min))
        (should (search-forward "*** Alpha" nil t))
        (should (search-forward "*** Beta" nil t))
        ;; Ссылки как таковые
        (goto-char (point-min))
        (should (re-search-forward "\\[\\[https://a\\]\\[A\\]\\]" nil t))
        (goto-char (point-min))
        (should (re-search-forward "\\[\\[https://b\\]\\[B\\]\\]" nil t))))))

;; Tag exclusion tests

(ert-deftest raindrop-parse-tags-with-exclusion-basic ()
  "Test basic tag exclusion parsing."
  (let ((result (raindrop-parse-tags-with-exclusion "tag1, -tag2, tag3")))
    (should (equal (plist-get result :tags) '("tag1" "tag3")))
    (should (equal (plist-get result :excluded-tags) '("tag2")))))

(ert-deftest raindrop-parse-tags-with-exclusion-no-exclusion ()
  "Test parsing without excluded tags."
  (let ((result (raindrop-parse-tags-with-exclusion "tag1, tag2")))
    (should (equal (plist-get result :tags) '("tag1" "tag2")))
    (should (equal (plist-get result :excluded-tags) nil))))

(ert-deftest raindrop-parse-tags-with-exclusion-only-exclusion ()
  "Test parsing with only excluded tags."
  (let ((result (raindrop-parse-tags-with-exclusion "-tag1, -tag2")))
    (should (equal (plist-get result :tags) nil))
    (should (equal (plist-get result :excluded-tags) '("tag1" "tag2")))))

(ert-deftest raindrop-parse-tags-with-exclusion-list-input ()
  "Test exclusion parsing with list input (no exclusion support)."
  (let ((result (raindrop-parse-tags-with-exclusion '("tag1" "tag2"))))
    (should (equal (plist-get result :tags) '("tag1" "tag2")))
    (should (equal (plist-get result :excluded-tags) nil))))

(ert-deftest raindrop-search-input-parsing-with-exclusion ()
  "Test search input parsing with excluded tags using -# syntax."
  (let ((parsed (raindrop--parse-search-input "#include -#exclude [folder] text")))
    (should (equal (plist-get parsed :tags) '("include")))
    (should (equal (plist-get parsed :excluded-tags) '("exclude")))
    (should (equal (plist-get parsed :folders) '("folder")))
    (should (equal (plist-get parsed :text) "text"))))

(ert-deftest raindrop-build-excluded-tag-search ()
  "Test building excluded tag search string."
  (should (equal (raindrop--build-excluded-tag-search '("tag1" "tag2"))
                 "-#tag1 -#tag2"))
  (should (equal (raindrop--build-excluded-tag-search '("spaced tag"))
                 "-#\"spaced tag\""))
  (should (equal (raindrop--build-excluded-tag-search nil) nil)))

(ert-deftest raindrop-compose-search-string-with-exclusion ()
  "Test composing search string with excluded tags."
  (should (string-match-p "#include.*-#exclude"
                          (raindrop--compose-search-string "text" '("include") '("exclude")))))

(ert-deftest raindrop-parse-tags-with-exclusion-spaced-tags ()
  "Test parsing tags with spaces using comma separation."
  ;; Test with comma-separated format (new org parsing function)
  (let ((result (raindrop-org--parse-tags-string "file manager, -outdated, cli")))
    (should (equal (plist-get result :tags) '("file manager" "cli")))
    (should (equal (plist-get result :excluded-tags) '("outdated")))))

(ert-deftest raindrop-build-excluded-tag-search-with-spaces ()
  "Test building excluded tag search string with spaced tags."
  (should (equal (raindrop--build-excluded-tag-search '("old tool"))
                 "-#\"old tool\"")))

;; Test org-babel parameter parsing

(ert-deftest raindrop-ob-comma-separated-tags ()
  "Test that ob-raindrop can handle comma-separated tags without quotes."
  (require 'ob-raindrop)
  ;; Test basic comma separation without spaces in tags
  (let ((params '((:tags . "cli, -windows, macos"))))
    (let* ((tags-input (raindrop-ob--param :tags params nil))
           (parsed-tags (raindrop-parse-tags-with-exclusion tags-input))
           (tags (plist-get parsed-tags :tags))
           (excluded-tags (plist-get parsed-tags :excluded-tags)))
      (should (equal tags '("cli" "macos")))
      (should (equal excluded-tags '("windows"))))))


(ert-deftest raindrop-org-parse-tags-string-with-spaces ()
  "Test new org tag parsing function with tags containing spaces."
  (let ((result (raindrop-org--parse-tags-string "cli, -openai, -tui with space")))
    (should (equal (plist-get result :tags) '("cli")))
    (should (equal (plist-get result :excluded-tags) '("openai" "tui with space"))))
  
  ;; Test space-separated format
  (let ((result (raindrop-org--parse-tags-string "cli -openai macos")))
    (should (equal (plist-get result :tags) '("cli" "macos")))
    (should (equal (plist-get result :excluded-tags) '("openai")))))

(ert-deftest raindrop-org-dblock-new-implementation ()
  "Test that new dblock implementation correctly parses parameters."
  (with-temp-buffer
    (org-mode)
    (insert "#+BEGIN: raindrop :tags \"cli, -openai, -tui with space\" :match all :limit 5\n#+END:\n")
    (goto-char (point-min))
    (let (parsed-tags parsed-excluded-tags)
      (cl-letf (((symbol-function 'raindrop-fetch)
                 (lambda (&rest plist)
                   (setq parsed-tags (plist-get plist :tags))
                   (setq parsed-excluded-tags (plist-get plist :excluded-tags))
                   nil)))
        (org-dblock-write:raindrop '(:tags "cli, -openai, -tui with space" :match all :limit 5))
        (should (equal parsed-tags '("cli")))
        (should (equal parsed-excluded-tags '("openai" "tui with space")))))))


;;; raindrop-core-tests.el ends here
