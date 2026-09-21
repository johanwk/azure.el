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

(ert-deftest azure-devops-link-search-results-extracts-id-and-title ()
  (let ((data
         '((results
            ((fields
              (System.Id . 796829)
              (System.WorkItemType . "Feature")
              (System.Title . "Integration with VIS")))))))
    (should
     (equal (azure-devops--link-search-results data)
            '(("796829" "Integration with VIS"))))))

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

;;; azure-devops-search-test.el ends here
