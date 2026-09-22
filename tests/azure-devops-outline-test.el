;;; azure-devops-outline-test.el --- Outline tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'azure-devops-outline)

(defun azure-devops-outline-test--item (id title type state &optional assignee)
  "Create a test work item."
  `((id . ,id)
    (fields
     (System.Id . ,id)
     (System.Title . ,title)
     (System.WorkItemType . ,type)
     (System.State . ,state)
     (System.AssignedTo . ,(and assignee `((displayName . ,assignee)))))))

(ert-deftest azure-devops-outline-extracts-tree-and-flat-ids ()
  (let ((tree '((workItemRelations
                 ((target (id . 1)))
                 ((source (id . 1)) (target (id . 2)))
                 ((source (id . 2)) (target (id . 3))))))
        (flat '((workItems ((id . 1)) ((id . 2)) ((id . 3)) ((id . 9))))))
    (should (equal (azure-devops-outline--edges tree) '((1 . 2) (2 . 3))))
    (should (equal (azure-devops-outline--all-ids tree flat) '(1 2 3 9)))))

(ert-deftest azure-devops-outline-chunks-batch-requests ()
  (should (equal (azure-devops-outline--chunks '(1 2 3 4 5) 2)
                 '((1 2) (3 4) (5)))))

(ert-deftest azure-devops-outline-fetches-vector-batches-as-list ()
  (let (success-callback)
    (cl-letf (((symbol-function 'azure-post)
               (lambda (_api success &rest _)
                 (setq success-callback success))))
      (let ((promise (azure-devops-outline--fetch-batch '(1 2))))
        (funcall success-callback
                 :data '((value . [((id . 1)) ((id . 2))])))
        (should
         (equal (promise-wait 1 promise)
                '(:fullfilled (((id . 1)) ((id . 2))))))))))

(ert-deftest azure-devops-outline-renders-hierarchy-and-unlinked-items ()
  (let* ((azure-project "Example")
         (items (list
                 (azure-devops-outline-test--item
                  1 "Main epic" "Epic" "Active" "Ada Lovelace")
                 (azure-devops-outline-test--item
                  2 "Child feature" "Feature" "New")
                 (azure-devops-outline-test--item
                  3 "Grandchild story" "User Story" "Closed")
                 (azure-devops-outline-test--item
                  9 "Needs a parent" "Task" "To Do")))
         (text (azure-devops-outline--render items '((1 . 2) (2 . 3)))))
    (should (string-match-p "#\\+TODO: ACTIVE NEW TO-DO | CLOSED" text))
    (should (string-match-p
             "\\*\\* ACTIVE \\[\\[azure-work-item:1\\]\\[Main epic\\]\\]" text))
    (should (string-match-p
             "\\*\\*\\* NEW \\[\\[azure-work-item:2\\]\\[Child feature\\]\\]" text))
    (should (string-match-p
             "\\*\\*\\*\\* CLOSED \\[\\[azure-work-item:3\\]\\[Grandchild story\\]\\]" text))
    (should (string-match-p
             "\\* Unlinked work items.*\\*\\* TO-DO \\[\\[azure-work-item:9\\]\\[Needs a parent\\]\\]"
             (replace-regexp-in-string "\n" " " text)))
    (should (string-match-p ":AZURE_ASSIGNEE: Ada Lovelace" text))))

(ert-deftest azure-devops-outline-mode-is-read-only-org ()
  (let ((buffer (generate-new-buffer " *azure-outline-test*")))
    (unwind-protect
        (with-current-buffer buffer
          (insert "* Test\n")
          (azure-devops-outline-mode)
          (should (derived-mode-p 'org-mode))
          (should buffer-read-only)
          (should-not buffer-offer-save))
      (kill-buffer buffer))))

(ert-deftest azure-devops-outline-return-opens-heading-work-item ()
  (with-temp-buffer
    (insert "* ACTIVE [[azure-work-item:42][Example task]]\n")
    (azure-devops-outline-mode)
    (goto-char (point-min))
    (should (eq (key-binding (kbd "RET"))
                #'azure-devops-outline-open-at-point))
    (should (eq (key-binding (kbd "<return>"))
                #'azure-devops-outline-open-at-point))
    (let (opened)
      (cl-letf (((symbol-function 'org-open-at-point)
                 (lambda (&rest _)
                   (setq opened (org-element-link-parser)))))
        ;; Exercise the command under the outline's real read-only setting.
        (call-interactively (key-binding (kbd "RET"))))
      (should (equal (org-element-property :type opened) "azure-work-item"))
      (should (equal (org-element-property :path opened) "42")))))

(ert-deftest azure-devops-outline-return-rejects-structural-heading ()
  (with-temp-buffer
    (org-mode)
    (insert "* Work-item hierarchy\n")
    (goto-char (point-min))
    (should-error (azure-devops-outline-open-at-point)
                  :type 'user-error)))

;;; azure-devops-outline-test.el ends here
