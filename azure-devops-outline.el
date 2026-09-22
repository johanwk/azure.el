;;; azure-devops-outline.el --- Read-only Org outline of Azure work items -*- coding: utf-8; lexical-binding: t; -*-
;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; Version: 2022.07.15
;; URL: https://github.com/hkjels/azure.el
;; Keywords: tools, azure, devops, outlines
;; Package-Requires: ((emacs "28.1") (azure-devops "2022.07.15"))

;; Copyright (C) 2026 Henrik Kjerringvåg
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Build a disposable, read-only Org snapshot of all work items in the current
;; Azure DevOps project.  Parent/Child relations determine outline nesting;
;; items with no Parent/Child relation are shown separately.

;;; Code:

(require 'azure-devops)
(require 'cl-lib)
(require 'org)
(require 'org-colview)
(require 'seq)
(require 'subr-x)

(defgroup azure-devops-outline nil
  "Read-only Org outlines of Azure DevOps work items."
  :prefix "azure-devops-outline-"
  :group 'azure-devops)

(defcustom azure-devops-outline-buffer "*Azure work items: %P*"
  "Name of the generated outline buffer.
%P is replaced with the current project name."
  :group 'azure-devops-outline
  :type 'string)

(defcustom azure-devops-outline-batch-size 200
  "Maximum number of work items fetched in one batch request."
  :group 'azure-devops-outline
  :type '(integer :tag "Batch size" 1))

(defcustom azure-devops-outline-columns-format
  "%45ITEM(Task) %12AZURE_TYPE(Type) %12AZURE_STATE(State) %28AZURE_ASSIGNEE(Assignee) %10AZURE_ID(ID)"
  "Column view format used in an Azure work-item outline."
  :group 'azure-devops-outline
  :type 'string)

(defconst azure-devops-outline--fields
  ["System.Id" "System.Title" "System.WorkItemType" "System.State"
   "System.AssignedTo"])

(defconst azure-devops-outline--completed-states
  '("closed" "done" "resolved" "removed" "completed")
  "Azure states displayed after Org's TODO separator.")

(defun azure-devops-outline--buffer-name ()
  "Return the outline buffer name for the current project."
  (replace-regexp-in-string
   "%P" (or azure-project "") azure-devops-outline-buffer t t))

(defun azure-devops-outline--wiql (query)
  "Return a promise resolving to the response for WIQL QUERY."
  (promise-new
   (lambda (resolve _reject)
     (azure-post
      "https://dev.azure.com/{organization}/{project}/_apis/wit/wiql"
      (cl-function (lambda (&key data &allow-other-keys)
                     (funcall resolve data)))
      `(("query" . ,query))
      '(("api-version" . "7.1"))))))

(defun azure-devops-outline--hierarchy-query ()
  "Return a promise resolving to the project's Parent/Child tree response."
  (azure-devops-outline--wiql
   (concat
    "SELECT [System.Id] FROM WorkItemLinks "
    "WHERE ([Source].[System.TeamProject] = @project) "
    "AND ([System.Links.LinkType] = "
    "'System.LinkTypes.Hierarchy-Forward') "
    "AND ([Target].[System.TeamProject] = @project) "
    "MODE (Recursive)")))

(defun azure-devops-outline--flat-query ()
  "Return a promise resolving to all work-item IDs in the current project."
  (azure-devops-outline--wiql
   (concat
    "SELECT [System.Id] FROM WorkItems "
    "WHERE [System.TeamProject] = @project "
    "ORDER BY [System.Id]")))

(defun azure-devops-outline--chunks (items size)
  "Split ITEMS into lists containing at most SIZE elements."
  (unless (> size 0)
    (error "Chunk size must be positive"))
  (let (chunks)
    (while items
      (let ((chunk nil))
        (dotimes (_ size)
          (when items
            (push (pop items) chunk)))
        (push (nreverse chunk) chunks)))
    (nreverse chunks)))

(defun azure-devops-outline--fetch-batch (ids)
  "Return a promise resolving to work items identified by IDS."
  (promise-new
   (lambda (resolve _reject)
     (azure-post
      "https://dev.azure.com/{organization}/{project}/_apis/wit/workitemsbatch"
      (cl-function
       (lambda (&key data &allow-other-keys)
         ;; JSON arrays may be decoded as vectors or lists, depending on the
         ;; caller's JSON settings.  Keep the rest of this module list-based.
         (funcall resolve (append (cdr (assoc 'value data)) nil))))
      `(("ids" . ,(vconcat ids))
        ("fields" . ,azure-devops-outline--fields))
      '(("api-version" . "7.1"))))))

(defun azure-devops-outline--fetch-items (ids)
  "Return a promise resolving to all work items in IDS."
  (if (null ids)
      (promise-resolve nil)
    (promise-then
     (promise-all
      (vconcat
       (mapcar #'azure-devops-outline--fetch-batch
               (azure-devops-outline--chunks
                ids azure-devops-outline-batch-size))))
     (lambda (batches)
       (apply #'append (append batches nil))))))

(defun azure-devops-outline--relation-id (side relation)
  "Return the work-item ID from SIDE of RELATION."
  (cdr (assoc 'id (cdr (assoc side relation)))))

(defun azure-devops-outline--edges (tree-data)
  "Return (PARENT . CHILD) edges found in TREE-DATA."
  (delq
   nil
   (mapcar
    (lambda (relation)
      (let ((parent (azure-devops-outline--relation-id 'source relation))
            (child (azure-devops-outline--relation-id 'target relation)))
        (when (and parent child) (cons parent child))))
    (cdr (assoc 'workItemRelations tree-data)))))

(defun azure-devops-outline--flat-ids (flat-data)
  "Return work-item IDs found in flat WIQL response FLAT-DATA."
  (delq nil
        (mapcar (lambda (item) (cdr (assoc 'id item)))
                (cdr (assoc 'workItems flat-data)))))

(defun azure-devops-outline--all-ids (tree-data flat-data)
  "Return unique IDs from TREE-DATA and FLAT-DATA."
  (delete-dups
   (append
    (azure-devops-outline--flat-ids flat-data)
    (apply #'append
           (mapcar (lambda (edge) (list (car edge) (cdr edge)))
                   (azure-devops-outline--edges tree-data))))))

(defun azure-devops-outline--field (item field)
  "Return FIELD from Azure work ITEM."
  (cdr (assoc field (cdr (assoc 'fields item)))))

(defun azure-devops-outline--assignee (item)
  "Return the display name of ITEM's assignee, or an empty string."
  (let ((value (azure-devops-outline--field item 'System.AssignedTo)))
    (cond ((stringp value) value)
          ((listp value) (or (cdr (assoc 'displayName value))
                             (cdr (assoc 'uniqueName value)) ""))
          (t ""))))

(defun azure-devops-outline--state-keyword (state)
  "Normalize Azure STATE to an Org TODO keyword."
  (let ((keyword (upcase
                  (replace-regexp-in-string
                   "[^[:alnum:]]+" "-" (string-trim (or state ""))))))
    (if (string-empty-p keyword) "UNKNOWN" keyword)))

(defun azure-devops-outline--completed-state-p (state)
  "Return non-nil when Azure STATE is normally a completed state."
  (member (downcase (or state ""))
          azure-devops-outline--completed-states))

(defun azure-devops-outline--todo-line (items)
  "Return a buffer-local Org TODO declaration based on ITEMS."
  (let (open done)
    (dolist (item items)
      (let* ((state (azure-devops-outline--field item 'System.State))
             (keyword (azure-devops-outline--state-keyword state)))
        (if (azure-devops-outline--completed-state-p state)
            (cl-pushnew keyword done :test #'equal)
          (cl-pushnew keyword open :test #'equal))))
    (setq open (sort open #'string-lessp)
          done (sort done #'string-lessp))
    (concat "#+TODO: "
            (string-join open " ")
            (if (and open done) " | " "")
            (string-join done " ") "\n")))

(defun azure-devops-outline--property-value (value)
  "Return VALUE as safe single-line Org property text."
  (replace-regexp-in-string "[\n\r]+" " " (format "%s" (or value ""))))

(defun azure-devops-outline--item-less-p (left right)
  "Return non-nil when work item LEFT should precede RIGHT."
  (let* ((order '("Epic" "Feature" "User Story" "Product Backlog Item"
                  "Requirement" "Bug" "Task"))
         (left-type (azure-devops-outline--field left 'System.WorkItemType))
         (right-type (azure-devops-outline--field right 'System.WorkItemType))
         (left-rank (or (cl-position left-type order :test #'equal) 99))
         (right-rank (or (cl-position right-type order :test #'equal) 99))
         (left-title (downcase (or (azure-devops-outline--field
                                    left 'System.Title) "")))
         (right-title (downcase (or (azure-devops-outline--field
                                     right 'System.Title) ""))))
    (or (< left-rank right-rank)
        (and (= left-rank right-rank)
             (or (string-lessp left-title right-title)
                 (and (string= left-title right-title)
                      (< (cdr (assoc 'id left))
                         (cdr (assoc 'id right)))))))))

(defun azure-devops-outline--insert-item (item level)
  "Insert ITEM as an Org heading at LEVEL."
  (let* ((id (cdr (assoc 'id item)))
         (title (azure-devops-outline--property-value
                 (azure-devops-outline--field item 'System.Title)))
         (type (azure-devops-outline--property-value
                (azure-devops-outline--field item 'System.WorkItemType)))
         (state (azure-devops-outline--property-value
                 (azure-devops-outline--field item 'System.State)))
         (assignee (azure-devops-outline--property-value
                    (azure-devops-outline--assignee item)))
         (keyword (azure-devops-outline--state-keyword state)))
    (insert (make-string level ?*) " " keyword " "
            (org-link-make-string (format "azure-work-item:%s" id) title)
            "\n:PROPERTIES:\n"
            ":AZURE_ID: " (format "%s" id) "\n"
            ":AZURE_TYPE: " type "\n"
            ":AZURE_STATE: " state "\n"
            ":AZURE_ASSIGNEE: " assignee "\n"
            ":END:\n")))

(defun azure-devops-outline--index-items (items)
  "Return a hash table mapping IDs to ITEMS."
  (let ((index (make-hash-table :test #'eql)))
    (dolist (item items index)
      (puthash (cdr (assoc 'id item)) item index))))

(defun azure-devops-outline--structure (items edges)
  "Build hierarchy metadata from ITEMS and EDGES.
Return a list (INDEX CHILDREN PARTICIPANTS ROOTS UNLINKED)."
  (let ((index (azure-devops-outline--index-items items))
        (children (make-hash-table :test #'eql))
        (participants (make-hash-table :test #'eql))
        (has-parent (make-hash-table :test #'eql)))
    (dolist (edge edges)
      (when (and (gethash (car edge) index) (gethash (cdr edge) index))
        (puthash (car edge) t participants)
        (puthash (cdr edge) t participants)
        (puthash (cdr edge) t has-parent)
        (cl-pushnew (cdr edge) (gethash (car edge) children) :test #'eql)))
    (let (roots unlinked)
      (maphash
       (lambda (id item)
         (cond ((and (gethash id participants) (not (gethash id has-parent)))
                (push item roots))
               ((not (gethash id participants)) (push item unlinked))))
       index)
      (list index children participants
            (sort roots #'azure-devops-outline--item-less-p)
            (sort unlinked #'azure-devops-outline--item-less-p)))))

(defun azure-devops-outline--insert-tree (item level index children visited)
  "Insert ITEM and descendants, guarding against cycles with VISITED."
  (let ((id (cdr (assoc 'id item))))
    (unless (gethash id visited)
      (puthash id t visited)
      (azure-devops-outline--insert-item item level)
      (dolist (child
               (sort (delq nil (mapcar (lambda (child-id)
                                         (gethash child-id index))
                                       (gethash id children)))
                     #'azure-devops-outline--item-less-p))
        (azure-devops-outline--insert-tree
         child (1+ level) index children visited)))))

(defun azure-devops-outline--render (items edges)
  "Return an Org outline representing ITEMS and Parent/Child EDGES."
  (pcase-let* ((`(,index ,children ,participants ,roots ,unlinked)
                 (azure-devops-outline--structure items edges))
               (visited (make-hash-table :test #'eql)))
    (with-temp-buffer
      (insert "#+title: Azure work items — " (or azure-project "") "\n"
              (azure-devops-outline--todo-line items)
              "#+columns: " azure-devops-outline-columns-format "\n\n"
              "* Work-item hierarchy\n")
      (dolist (root roots)
        (azure-devops-outline--insert-tree root 2 index children visited))
      ;; Malformed cycles have no root.  Keep their items visible rather than
      ;; silently dropping them from the snapshot.
      (let (remaining)
        (maphash (lambda (id _)
                   (unless (gethash id visited)
                     (push (gethash id index) remaining)))
                 participants)
        (dolist (item (sort (delq nil remaining)
                            #'azure-devops-outline--item-less-p))
          (azure-devops-outline--insert-tree
           item 2 index children visited)))
      (insert "\n* Unlinked work items\n")
      (dolist (item unlinked)
        (azure-devops-outline--insert-item item 2))
      (buffer-string))))

(defun azure-devops-outline-open-at-point ()
  "Open the Azure work item represented by the heading at point.

This also works while Org column view is active.  Signal a user error when
point is on a structural heading rather than a work-item heading."
  (interactive)
  (org-with-wide-buffer
   (org-back-to-heading t)
   (let ((end (line-end-position))
         link-position)
     (while (and (not link-position)
                 (re-search-forward org-link-bracket-re end t))
       (when (string-prefix-p "azure-work-item:" (match-string-no-properties 1))
         (setq link-position (match-beginning 0))))
     (unless link-position
       (user-error "No Azure work item on this heading"))
     (goto-char link-position)
     (org-open-at-point))))

(defvar azure-devops-outline-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    map)
  "Keymap for `azure-devops-outline-mode'.")

;; Define these outside the `defvar' initializer so that reloading this
;; library also updates a keymap created by an older version.
(define-key azure-devops-outline-mode-map
            (kbd "RET") #'azure-devops-outline-open-at-point)
(define-key azure-devops-outline-mode-map
            (kbd "<return>") #'azure-devops-outline-open-at-point)
(define-key azure-devops-outline-mode-map
            (kbd "g") #'azure-devops-work-item-outline)
(define-key azure-devops-outline-mode-map (kbd "q") #'quit-window)

(define-derived-mode azure-devops-outline-mode org-mode "Azure-Outline"
  "Major mode for a disposable Azure DevOps work-item outline."
  (setq-local org-columns-default-format azure-devops-outline-columns-format)
  ;; Column view supplies its own keymap, where `g' normally redraws columns.
  ;; Use a buffer-local copy so `g' refreshes only Azure outline buffers.
  (setq-local org-columns-map (copy-keymap org-columns-map))
  (define-key org-columns-map (kbd "g") #'azure-devops-work-item-outline)
  (setq-local buffer-offer-save nil)
  (setq buffer-read-only t))

(defun azure-devops-outline--display (items edges)
  "Render ITEMS and EDGES in a read-only Org buffer and display it."
  (let ((content (azure-devops-outline--render items edges))
        (buffer (get-buffer-create (azure-devops-outline--buffer-name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (goto-char (point-min)))
      (azure-devops-outline-mode)
      ;; Apply the file-wide TODO declaration before fontifying headings.
      (org-set-regexps-and-options)
      (font-lock-flush (point-min) (point-max))
      (font-lock-ensure (point-min) (point-max))
      ;; Present the snapshot as a table of contents: show every heading while
      ;; keeping their bodies and property drawers folded.
      (org-content)
      (goto-char (point-min))
      (set-buffer-modified-p nil))
    ;; Display after the async continuation has returned, so its window-state
    ;; restoration does not immediately undo `pop-to-buffer'.
    (run-at-time 0 nil #'pop-to-buffer buffer)
    buffer))

;;;###autoload
(async-defun azure-devops-work-item-outline ()
  "Display all project work items as a read-only Org hierarchy snapshot."
  (interactive)
  (unless (azure--valid-p)
    (user-error "You need to run `azure-init` first!"))
  (message "Retrieving Azure work-item hierarchy...")
  (let* ((responses (await
                     (promise-all
                      (vector (azure-devops-outline--hierarchy-query)
                              (azure-devops-outline--flat-query)))))
         (tree-data (aref responses 0))
         (flat-data (aref responses 1))
         (edges (azure-devops-outline--edges tree-data))
         (ids (azure-devops-outline--all-ids tree-data flat-data))
         (items (await (azure-devops-outline--fetch-items ids))))
    (azure-devops-outline--display items edges)
    (message "Displayed %d Azure work items (%d hierarchy links)"
             (length items) (length edges))))

(provide 'azure-devops-outline)
;;; azure-devops-outline.el ends here
