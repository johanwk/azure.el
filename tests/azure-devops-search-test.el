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
               (lambda (_url _success &optional data _params _headers)
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

(ert-deftest azure-devops-parse-team-members-preserves-full-identity ()
  (let ((data '(((identity
                  (displayName . "Klüwer, Johan Wilhelm")
                  (uniqueName . "johan@example.org")
                  (imageUrl . "https://example.org/avatar"))))))
    (should
     (equal (azure-devops--parse-team-members data)
            '(("Klüwer, Johan Wilhelm <johan@example.org>"
               . "https://example.org/avatar"))))))

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
            (System.CreatedBy (displayName . "Grace Hopper")))))))
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
                 (lambda (method api success params data headers)
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
                 (lambda (_method _api success _params data _headers)
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
