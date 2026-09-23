;;; azure-devops-search-test.el --- Search request tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'azure-devops)

(defun azure-devops-test--search-request (assignees)
  "Return the JSON-decoded search request for ASSIGNEES, without networking."
  (let ((azure-project "Example")
        (azure--keywords nil)
        (azure--types nil)
        (azure--assignees assignees)
        (azure--state nil)
        (azure--area nil)
        (azure--iteration nil)
        (azure--tags nil)
        (json-object-type 'hash-table)
        (json-array-type 'list)
        captured)
    (cl-letf (((symbol-function 'azure-post)
               (lambda (_url _success &optional data _params _headers _error)
                 ;; Mirror the single encoding performed by `azure-req'.
                 (setq captured (json-read-from-string (json-encode data))))))
      (azure-devops--search))
    captured))

(ert-deftest azure-devops-search-filter-json-shape ()
  (let* ((request (azure-devops-test--search-request '("Ada Lovelace")))
         (filters (gethash "filters" request)))
    (should (hash-table-p filters))
    (should (equal (gethash "System.TeamProject" filters) '("Example")))
    (should (equal (gethash "System.AssignedTo" filters) '("Ada Lovelace")))
    (should (eq (gethash "includeFacets" request) t))))

(ert-deftest azure-devops-search-multiple-assignees ()
  (let* ((names '("Ada Lovelace" "Grace Hopper"))
         (request (azure-devops-test--search-request names)))
    (should (equal (gethash "System.AssignedTo" (gethash "filters" request))
                   names))))

(ert-deftest azure-devops-search-no-assignee-filter ()
  (let ((filters (gethash "filters" (azure-devops-test--search-request nil))))
    (should (hash-table-p filters))
    (should (eq (gethash "System.AssignedTo" filters 'absent) 'absent))))

(ert-deftest azure-devops-search-handles-unsupported-filter ()
  (let ((azure--iteration '("Example\\Sprint 1")))
    (should
     (equal
      (azure-devops--search-error
       :data '((message . "Unknown filter [System.IterationPath] found."))
       :error-thrown '(error http 400))
      "System.IterationPath"))
    (should-not azure--iteration)))

(ert-deftest azure-devops-search-still-signals-other-errors ()
  (should-error
   (azure-devops--search-error
    :data '((message . "Service unavailable"))
    :error-thrown '(error http 503)))
  ;; Do not retry indefinitely if Azure rejects a field that was not selected.
  (let ((azure--iteration nil))
    (should-error
     (azure-devops--search-error
      :data '((message . "Unknown filter [System.IterationPath] found."))
      :error-thrown '(error http 400)))))

(ert-deftest azure-devops-search-retries-after-unsupported-filter ()
  (let ((azure-project "Example")
        (azure--keywords nil)
        (azure--types nil)
        (azure--assignees nil)
        (azure--state nil)
        (azure--area nil)
        (azure--iteration '("Example\\Sprint 1"))
        (azure--tags nil)
        error-handler
        requests)
    (cl-letf (((symbol-function 'azure-post)
               (lambda (_url _success &optional data _params _headers error)
                 (push data requests)
                 (setq error-handler error))))
      (azure-devops--search "query" 0)
      (let ((first-error-handler error-handler))
        (funcall first-error-handler
                 :data '((message . "Unknown filter [System.IterationPath] found."))
                 :error-thrown '(error http 400)))
      (should (= (length requests) 2))
      (should-not azure--iteration)
      (should
       (eq (gethash "System.IterationPath"
                    (cdr (assoc "filters" (car requests))) 'absent)
           'absent)))))

(ert-deftest azure-devops-search-displays-friendly-unsupported-filter-message ()
  (let ((azure-project "Example")
        (azure--keywords nil)
        (azure--types nil)
        (azure--assignees nil)
        (azure--state nil)
        (azure--area nil)
        (azure--iteration '("Example"))
        (azure--tags nil)
        error-handler
        shown)
    (cl-letf (((symbol-function 'azure-post)
               (lambda (_url _success &optional _data _params _headers error)
                 (setq error-handler error)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq shown (apply #'format format-string args)))))
      (azure-devops--search)
      (let ((handler error-handler))
        (funcall handler
                 :data '((message . "Unknown filter [System.IterationPath] found."))
                 :error-thrown '(error http 400)))
      (should (string-match-p "System.IterationPath is not available" shown)))))

(ert-deftest azure-devops-search-removes-successive-unsupported-filters ()
  (let ((azure-project "Example")
        (azure--keywords nil)
        (azure--types nil)
        (azure--assignees nil)
        (azure--state nil)
        (azure--area '("Example"))
        (azure--iteration '("Example"))
        (azure--tags nil)
        error-handler
        (request-count 0))
    (cl-letf (((symbol-function 'azure-post)
               (lambda (_url _success &optional _data _params _headers error)
                 (cl-incf request-count)
                 (setq error-handler error))))
      (azure-devops--search)
      (let ((handler error-handler))
        (funcall handler
                 :data '((message . "Unknown filter [System.IterationPath] found."))
                 :error-thrown '(error http 400)))
      (let ((handler error-handler))
        (funcall handler
                 :data '((message . "Unknown filter [System.AreaPath] found."))
                 :error-thrown '(error http 400)))
      (should (= request-count 3))
      (should-not azure--iteration)
      (should-not azure--area))))

(ert-deftest azure-devops-search-list-valued-filters ()
  (let ((azure-project "Example")
        (azure--keywords nil)
        (azure--types nil)
        (azure--assignees nil)
        (azure--state '("Active" "New"))
        (azure--area '("Example\\Platform"))
        (azure--iteration '("Example\\Sprint 1"))
        (azure--tags '("Backend" "Urgent")))
    (let ((filters (azure-devops--build-filter-object)))
      (should (equal (gethash "System.State" filters) azure--state))
      (should (equal (gethash "System.AreaPath" filters) azure--area))
      (should (equal (gethash "System.IterationPath" filters)
                     azure--iteration))
      (should (equal (gethash "System.Tags" filters) azure--tags)))))

(ert-deftest azure-devops-parses-filter-values ()
  (should (equal (azure-devops--parse-states
                  '(((name . "New")) ((name . "Active"))
                    ((name . "New"))))
                 '("Active" "New")))
  (should (equal (azure-devops--parse-tags
                  '(((name . "Urgent")) ((name . "Backend"))))
                 '("Backend" "Urgent")))
  (should
   (equal
    (azure-devops--classification-paths
     '((name . "Example")
       (children .
        [((name . "Platform")
          (children . [((name . "API"))]))])))
    '("Example" "Example\\Platform" "Example\\Platform\\API"))))

(ert-deftest azure-devops-fetch-classification-paths-uses-depth ()
  (let (url params callback-value)
    (cl-letf (((symbol-function 'azure-get)
               (lambda (api success request-params)
                 (setq url api params request-params)
                 (funcall success
                          :data '((name . "Example")
                                  (children . [((name . "Platform"))]))))))
      (azure-devops--fetch-classification-paths
       "Areas" (lambda (value) (setq callback-value value)))
      (should (string-suffix-p "/classificationnodes/Areas" url))
      (should (equal params '(("$depth" . 20) ("api-version" . "7.1"))))
      (should (equal callback-value '("Example" "Example\\Platform"))))))

(ert-deftest azure-devops-menu-dispatches-all-filter-types ()
  (let (called)
    (cl-letf (((symbol-function 'azure-devops--get-available-types)
               (lambda () (push 'type called)))
              ((symbol-function 'azure-devops--get-available-team-members)
               (lambda () (push 'assignees called)))
              ((symbol-function 'azure-devops--get-available-states)
               (lambda () (push 'state called)))
              ((symbol-function 'azure-devops--get-available-areas)
               (lambda () (push 'area called)))
              ((symbol-function 'azure-devops--get-available-iterations)
               (lambda () (push 'iteration called)))
              ((symbol-function 'azure-devops--get-available-tags)
               (lambda () (push 'tags called))))
      (dolist (type '("type" "assignees" "state" "area" "iteration" "tags"))
        (azure-devops--menu type))
      (should (equal (nreverse called)
                     '(type assignees state area iteration tags))))))

(ert-deftest azure-devops-fetch-states-combines-work-item-types ()
  (let (requests result)
    (cl-letf (((symbol-function 'azure-devops--fetch-work-item-types)
               (lambda (callback) (funcall callback '("Task" "Bug"))))
              ((symbol-function 'azure-get)
               (lambda (url success _params)
                 (push url requests)
                 (funcall success
                          :data (if (string-match-p "Task/states" url)
                                    '((value . [((name . "New"))
                                                ((name . "Active"))]))
                                  '((value . [((name . "New"))
                                              ((name . "Closed"))])))))))
      (azure-devops--fetch-states (lambda (states) (setq result states)))
      (should (= (length requests) 2))
      (should (equal result '("Active" "Closed" "New"))))))

(ert-deftest azure-devops-parse-team-members-preserves-full-identity ()
  (let ((data '(((identity
                  (displayName . "Klüwer, Johan Wilhelm")
                  (uniqueName . "johan@example.org")
                  (imageUrl . "https://example.org/avatar"))))))
    (should
     (equal (azure-devops--parse-team-members data)
            '(("Klüwer, Johan Wilhelm <johan@example.org>"
               . "https://example.org/avatar"))))))

(ert-deftest azure-devops-filter-picker-uses-semicolons ()
  (let (selected searched)
    (cl-letf (((symbol-function 'completing-read-multiple)
               (lambda (_prompt values &rest _)
                 (should (equal crm-separator "[ \t]*;[ \t]*"))
                 (should (equal values '("One" "Two")))
                 '("Two")))
              ((symbol-function 'azure-devops--search)
               (lambda (&rest _) (setq searched t))))
      (azure-devops--select-filter-values
       "Select: " '("One" "Two")
       (lambda (values) (setq selected values)))
      (should (equal selected '("Two")))
      (should searched))))

(ert-deftest azure-devops-type-picker-waits-for-results ()
  (let ((azure--types nil)
        callback picker-called searched)
    (cl-letf (((symbol-function 'azure-devops--fetch-work-item-types)
               (lambda (function) (setq callback function)))
              ((symbol-function 'azure-devops--select-filter-values)
               (lambda (_prompt values setter)
                 (setq picker-called values)
                 (funcall setter '("Task"))))
              ((symbol-function 'azure-devops--search)
               (lambda (&rest _) (setq searched t))))
      (azure-devops--get-available-types)
      (should-not picker-called)
      (funcall callback '("Bug" "Task"))
      (should (equal picker-called '("Bug" "Task")))
      (should (equal azure--types '("Task")))
      ;; The shared picker normally performs this refresh; it is mocked above.
      (should-not searched))))

(ert-deftest azure-devops-assignee-picker-waits-and-preserves-commas ()
  (let ((members '(("Klüwer, Johan Wilhelm <johan@example.org>" . "avatar")
                   ("Hopper, Grace <grace@example.org>" . "avatar")))
        (azure--available-team-members '(("Stale Person" . "avatar")))
        (azure--assignees nil)
        callback
        picker-called
        searched)
    (cl-letf (((symbol-function 'azure--team-members)
               (lambda (function) (setq callback function)))
              ((symbol-function 'completing-read-multiple)
               (lambda (_prompt collection &rest _)
                 (setq picker-called t)
                 (should (equal crm-separator "[ \t]*;[ \t]*"))
                 (should (equal collection (mapcar #'car members)))
                 (mapcar #'car members)))
              ((symbol-function 'azure-devops--search)
               (lambda (&rest _) (setq searched t))))
      (azure-devops--get-available-team-members)
      (should-not picker-called)
      (should-not searched)
      (funcall callback members)
      (should picker-called)
      (should searched)
      (should (equal azure--available-team-members members))
      (should (equal azure--assignees (mapcar #'car members))))))

(ert-deftest azure-devops-relation-work-item-id ()
  (should (= (azure-devops--relation-work-item-id
              '((url . "https://dev.azure.com/example/_apis/wit/workItems/42")))
             42))
  (should-not
   (azure-devops--relation-work-item-id
    '((url . "https://dev.azure.com/example/_apis/git/repositories/1")))))

(ert-deftest azure-devops-work-item-relations-description-list ()
  (let* ((parent '((rel . "System.LinkTypes.Hierarchy-Reverse")
                   (attributes (name . "Parent"))
                   (url . "https://example/_apis/wit/workItems/7")))
         (child '((rel . "System.LinkTypes.Hierarchy-Forward")
                  (attributes (name . "Child"))
                  (url . "https://example/_apis/wit/workItems/9")))
         (formatted
          (azure-devops--work-item-relations
           (list
            (list parent '((id . 7)
                           (fields (System.Title . "Parent title"))))
            (list child '((id . 9)
                          (fields (System.Title . "Child title"))))))))
    (should
     (equal formatted
            (concat
             "- Parent :: [[azure-work-item:7][Parent title]]\n"
             "- Child :: [[azure-work-item:9][Child title]]\n\n")))))

(ert-deftest azure-devops-work-item-relations-sorts-parent-first ()
  (let* ((make-entry
          (lambda (type id title)
            (list `((attributes (name . ,type))
                    (url . ,(format "https://example/_apis/wit/workItems/%s" id)))
                  `((id . ,id)
                    (fields (System.Title . ,title))))))
         (formatted
          (azure-devops--work-item-relations
           (list (funcall make-entry "Related" 12 "Zulu")
                 (funcall make-entry "Child" 10 "Zulu child")
                 (funcall make-entry "Parent" 7 "Parent title")
                 (funcall make-entry "Child" 9 "Alpha child")
                 (funcall make-entry "Duplicate" 11 "Duplicate title")))))
    (should
     (equal formatted
            (concat
             "- Parent :: [[azure-work-item:7][Parent title]]\n"
             "- Child :: [[azure-work-item:9][Alpha child]]\n"
             "- Child :: [[azure-work-item:10][Zulu child]]\n"
             "- Duplicate :: [[azure-work-item:11][Duplicate title]]\n"
             "- Related :: [[azure-work-item:12][Zulu]]\n\n")))))

(ert-deftest azure-devops-work-item-links-final-section ()
  (let* ((azure-organization "Example Org")
         (azure-project "Example Project")
         (work-item '((id . 42)
                      (fields (System.Title . "Current title"))))
         (parent '((rel . "System.LinkTypes.Hierarchy-Reverse")
                   (attributes (name . "Parent"))
                   (url . "https://example/_apis/wit/workItems/7"))))
    (should
     (equal
      (azure-devops--work-item-links
       work-item
       (list (list parent '((id . 7)
                            (fields (System.Title . "Parent title"))))))
      (concat
       "* Links\n\n"
       "[[https://dev.azure.com/Example%20Org/Example%20Project/_workitems/edit/42][Open in Azure DevOps]]\n\n"
       "- Parent :: [[azure-work-item:7][Parent title]]\n\n")))))

(ert-deftest azure-devops-follow-work-item-link ()
  (let (opened)
    (cl-letf (((symbol-function 'azure-devops-work-item)
               (lambda (id) (setq opened id))))
      (azure-devops--follow-work-item-link "796829" nil)
      (should (= opened 796829)))))

(ert-deftest azure-devops-work-item-get-allows-missing-item ()
  (let (error-handler resolved done)
    (cl-letf (((symbol-function 'azure-get)
               (lambda (_api _success _params error)
                 (setq error-handler error)))
              ((symbol-function 'request-response-p) (lambda (_) t))
              ((symbol-function 'request-response-status-code) (lambda (_) 404)))
      (promise-then
       (azure-devops--work-item-get 42 t)
       (lambda (value) (setq resolved value done t)))
      (funcall error-handler :response 'response :error-thrown '(error http 404))
      (while (not done)
        (accept-process-output nil 0.01))
      (should-not resolved))))

(ert-deftest azure-devops-missing-work-item-shows-friendly-message ()
  (let (shown done)
    (cl-letf (((symbol-function 'azure-devops--work-item-get)
               (lambda (id missing-ok)
                 (should (= id 42))
                 (should missing-ok)
                 (promise-resolve nil)))
              ((symbol-function 'message)
               (lambda (format-string &rest args)
                 (setq shown (apply #'format format-string args)))))
      (promise-then (azure-devops--update-work-item-buffer 42)
                    (lambda (_) (setq done t)))
      (while (not done)
        (accept-process-output nil 0.01))
      (should (string-match-p "may have been deleted" shown)))))

(ert-deftest azure-devops-link-search-results-extracts-id-and-title ()
  (let ((data
         '((results
            ((fields
              (System.Id . 796829)
              (System.WorkItemType . "Feature")
              (System.Title . "Integration with VIS")
              (System.State . "Active")
              (System.AssignedTo
               (displayName . "Klüwer, Johan Wilhelm")
               (uniqueName . "johan@example.org"))))))))
    (should
     (equal (azure-devops--link-search-results data)
            '(("796829" "Integration with VIS" "Active"
               "Klüwer, Johan Wilhelm"))))))

(ert-deftest azure-devops-work-item-candidate-shows-useful-fields ()
  (should
   (equal
    (azure-devops--work-item-candidate
     '("796829" "Integration with VIS" "Active" "Ada Lovelace"))
    "Active      Ada Lovelace                  Integration with VIS  (#796829)")))

(ert-deftest azure-devops-read-work-item-id-skips-search-prompt ()
  (let ((azure-organization "Example")
        (azure-project "Project")
        (azure-team "Team")
        query)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _)
                 (ert-fail "Work-item selection should not prompt for a query")))
              ((symbol-function 'azure-devops--wait-for-link-search)
               (lambda (value)
                 (setq query value)
                 '(("796829" "Integration with VIS" "Active" "Ada Lovelace"))))
              ((symbol-function 'azure-devops--select-work-item)
               (lambda (items) (car items))))
      (should (= (azure-devops--read-work-item-id) 796829))
      (should (equal query "")))))

(ert-deftest azure-devops-interactive-work-item-uses-completion ()
  (let (opened)
    (cl-letf (((symbol-function 'azure-devops--read-work-item-id)
               (lambda () 796829))
              ((symbol-function 'azure-devops--update-work-item-buffer)
               (lambda (id) (setq opened id))))
      (call-interactively #'azure-devops-work-item)
      (should (= opened 796829)))))

(ert-deftest azure-devops-interactive-create-fetches-project-types ()
  (let ((azure-organization "Example")
        (azure-project "Project")
        (azure-team "Team")
        callback prompted created)
    (cl-letf (((symbol-function 'azure-devops--fetch-work-item-types)
               (lambda (function) (setq callback function))))
      (call-interactively #'azure-devops-work-item-create))
    (should (eq callback #'azure-devops--prompt-work-item-create))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (prompt collection &rest _)
                 (if (string-prefix-p "Item type" prompt)
                     (progn (setq prompted collection) "User Story")
                   (caar collection))))
              ((symbol-function 'read-from-minibuffer)
               (lambda (&rest _) "New story"))
              ((symbol-function 'azure-devops--wait-for-link-search)
               (lambda (_query)
                 '(("42" "Parent" "Active" "Ada Lovelace"))))
              ((symbol-function 'azure-devops-work-item-create)
               (lambda (type title parent)
                 (setq created (list type title parent)))))
      (funcall callback '("Bug" "User Story")))
    (should (equal prompted '("Bug" "User Story")))
    (should (equal created '("User Story" "New story" 42)))))

(ert-deftest azure-devops-create-parent-id-from-work-item-document ()
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: 796818\n:END:\n\n* Task\n")
    (should (= (azure-devops--create-parent-id) 796818))))

(ert-deftest azure-devops-create-parent-id-from-outline-heading ()
  (with-temp-buffer
    (rename-buffer "*Azure work items: Project*" t)
    (org-mode)
    (insert "* Project\n** NEW Task\n:PROPERTIES:\n:AZURE_ID: 798412\n:END:\n")
    (goto-char (point-max))
    (should (= (azure-devops--create-parent-id) 798412))))

(ert-deftest azure-devops-select-work-item-preselects-parent ()
  (let (default)
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _collection _predicate _require-match
                        _initial _history supplied-default &rest _)
                 (setq default supplied-default)
                 supplied-default)))
      (should
       (equal (car (azure-devops--select-work-item
                    '(("7" "Other" "New" "")
                      ("42" "Parent" "Active" "Ada Lovelace"))
                    42 "Parent: "))
              "42")))
    (should (string-match-p "Parent.*#42" default))))

(ert-deftest azure-devops-create-encodes-work-item-type ()
  (let ((azure-organization "Example")
        (azure-project "Project")
        (azure-team "Team")
        captured-url captured-data)
    (cl-letf (((symbol-function 'azure-post)
               (lambda (url _success &optional data _params _headers _error)
                 (setq captured-url url
                       captured-data data))))
      (azure-devops-work-item-create "User Story" "New story" 42)
      (should (string-suffix-p "/$User%20Story" captured-url))
      (should (equal (cdr (assoc "value" (car captured-data)))
                     "New story"))
      (let* ((relation-operation (cadr captured-data))
             (relation (cdr (assoc "value" relation-operation))))
        (should (equal (cdr (assoc "path" relation-operation))
                       "/relations/-"))
        (should (equal (cdr (assoc "rel" relation))
                       "System.LinkTypes.Hierarchy-Reverse"))
        (should (equal (cdr (assoc "url" relation))
                       "https://dev.azure.com/Example/_apis/wit/workItems/42"))))))

(ert-deftest azure-devops-link-search-uses-case-insensitive-prefixes ()
  ;; Azure Search interprets a trailing `*' as a prefix wildcard and
  ;; performs text matching case-insensitively.
  (should (equal (azure-devops--link-search-text "Ontol") "Ontol*"))
  (should (equal (azure-devops--link-search-text "semantic ontol")
                 "semantic* ontol*"))
  (should (equal (azure-devops--link-search-text "") "NOT null"))
  ;; Do not rewrite explicit Azure Search syntax.
  (should (equal (azure-devops--link-search-text "title:\"Ontology\"")
                 "title:\"Ontology\"")))

(ert-deftest azure-devops-insert-work-item-link-completion ()
  (with-temp-buffer
    (org-mode)
    (let ((marker (copy-marker (point) t)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (caar collection))))
        (azure-devops--insert-selected-work-item-link
         marker '(("796829" "Integration with VIS"))))
      (should
       (equal (buffer-string)
              "[[azure-work-item:796829][Integration with VIS]]")))))

(ert-deftest azure-devops-work-item-link-registers-completion ()
  (should
   (eq (org-link-get-parameter "azure-work-item" :complete)
       #'azure-devops--complete-work-item-link))
  (should
   (eq (org-link-get-parameter "azure-work-item" :insert-description)
       #'azure-devops--work-item-link-description)))

(ert-deftest azure-devops-work-item-link-completion-returns-string ()
  (let ((azure-organization "Example")
        (azure-project "Project")
        (azure-team "Team")
        (azure-devops--work-item-link-titles (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "integration"))
              ((symbol-function 'azure-devops--wait-for-link-search)
               (lambda (query)
                 (should (equal query "integration"))
                 '(("796829" "Integration with VIS"))))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (caar collection))))
      (let ((location (azure-devops--complete-work-item-link)))
        (should (stringp location))
        (should (equal location "azure-work-item:796829"))
        (should
         (equal (azure-devops--work-item-link-description location nil)
                "Integration with VIS"))))))

(ert-deftest azure-devops-work-item-properties-include-assignee ()
  (let ((work-item
         '((id . 42)
           (rev . 7)
           (fields
            (System.State . "Active")
            (System.AssignedTo (displayName . "Ada Lovelace"))
            (System.CreatedDate . "2026-09-22")
            (System.CreatedBy (displayName . "Grace Hopper"))))))
    (should (string-match-p
             "^:assignee: Ada Lovelace$"
             (azure-devops--work-item-properties work-item)))))

(ert-deftest azure-devops-first-heading-title-excludes-org-metadata ()
  (with-temp-buffer
    (org-mode)
    (insert "* TODO [#A] Updated task title :azure:\nBody.\n")
    (should (equal (azure-devops--first-heading-title)
                   "Updated task title"))))

(ert-deftest azure-devops-first-heading-description-uses-heading-body ()
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: 42\n:REV: 7\n:END:\n\n"
            "* TODO Example\n"
            "First paragraph.\n\n"
            "- item\n\n"
            "** Description subheading\nNested description.\n\n"
            "* Discussion (1)\nNot part of the description.\n")
    (should
     (equal (azure-devops--first-heading-description)
            (concat "First paragraph.\n\n- item\n\n"
                    "** Description subheading\nNested description.")))))

(ert-deftest azure-devops-update-title-description-and-state-sends-json-patch ()
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: 42\n:REV: 7\n:STATE: New\n:END:\n"
            "#+TODO: NEW ACTIVE | RESOLVED CLOSED REMOVED\n\n"
            "* ACTIVE Updated task title\n"
            "Updated *description*.\n\n"
            "* Discussion (0)\n\n"
            "* Personal Notes\nLocal only.\n")
    (org-set-regexps-and-options)
    (let ((azure-organization "Example")
          (azure-project "Project")
          (azure-team "Team")
          captured)
      (cl-letf (((symbol-function 'azure--org-to-html)
                 (lambda (description)
                   (should (equal description "Updated *description*."))
                   "<p>Updated <strong>description</strong>.</p>"))
                ((symbol-function 'azure-req)
                 (lambda (method api success params data headers &optional _error-handler)
                   (setq captured
                         (list method api params data headers))
                   (funcall success :data '((rev . 8)))
                   'request)))
        (should (eq (azure-devops-update-work-item-description) 'request))
        (should (equal (nth 0 captured) "PATCH"))
        (should (equal (nth 1 captured)
                       "https://dev.azure.com/{organization}/{project}/_apis/wit/workitems/42"))
        (should (equal (nth 2 captured) '(("api-version" . "7.1"))))
        (should
         (equal
          (nth 3 captured)
          '((("op" . "test") ("path" . "/rev") ("value" . 7))
            (("op" . "add")
             ("path" . "/fields/System.Title")
             ("value" . "Updated task title"))
            (("op" . "add")
             ("path" . "/fields/System.Description")
             ("value" . "<p>Updated <strong>description</strong>.</p>"))
            (("op" . "add")
             ("path" . "/fields/System.State")
             ("value" . "Active")))))
        (should
         (equal (nth 4 captured)
                '(("Content-Type" . "application/json-patch+json"))))
        (should (equal (azure-devops--document-property "rev") "8"))
        (should (equal (azure-devops--document-property "state") "Active"))))))

(ert-deftest azure-devops-update-without-todo-leaves-state-unchanged ()
  (with-temp-buffer
    (org-mode)
    (insert ":PROPERTIES:\n:ID: 42\n:REV: 7\n:STATE: New\n:END:\n"
            "#+TODO: NEW ACTIVE | RESOLVED CLOSED REMOVED\n\n"
            "* Updated task title\nDescription.\n")
    (org-set-regexps-and-options)
    (let ((azure-organization "Example")
          (azure-project "Project")
          (azure-team "Team")
          captured)
      (cl-letf (((symbol-function 'azure--org-to-html) #'identity)
                ((symbol-function 'azure-req)
                 (lambda (_method _api success _params data _headers &optional _error-handler)
                   (setq captured data)
                   (funcall success :data '((rev . 8)))
                   'request)))
        (azure-devops-update-work-item-description)
        (should-not
         (seq-find (lambda (operation)
                     (equal (cdr (assoc "path" operation))
                            "/fields/System.State"))
                   captured))
        (should (equal (azure-devops--document-property "state") "New"))))))

(ert-deftest azure-devops-work-item-title-uses-azure-state-keyword ()
  (let ((work-item '((fields (System.State . "Resolved")
                              (System.Title . "Example task")))))
    (should (equal azure-devops-work-item-todo-directive
                   "#+TODO: NEW ACTIVE | RESOLVED CLOSED REMOVED\n"))
    (should (equal (azure-devops--work-item-title work-item)
                   "* RESOLVED Example task\n"))))

(ert-deftest azure-devops-first-heading-state-allows-no-keyword ()
  (with-temp-buffer
    (org-mode)
    (insert "#+TODO: NEW ACTIVE | RESOLVED CLOSED REMOVED\n\n"
            "* Task without a state keyword\n")
    (org-set-regexps-and-options)
    (should-not (azure-devops--first-heading-state))))

;;; azure-devops-search-test.el ends here
