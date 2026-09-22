;;; azure-devops.el --- Azure-devops & org-mode working in symphony -*- coding: utf-8; lexical-binding: t; -*-
;; Author: Henrik Kjerringvåg <henrik@kjerringvag.no>
;; Version: 2022.07.15
;; URL: https://github.com/hkjels/azure-devops.el
;; Keywords: tools, azure, devops
;; Package-Requires: ((emacs "28.1") (azure "2022.07.15") (a "1.0.0") (dash "2.19.1") (s "1.12.0") (all-the-icons "5.0.0") (svg-lib "0.2.5"))

;; Copyright (C) <<year()>> Henrik Kjerringvåg
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
;;; Find tasks, discuss, record work etc. It's all working together with
;; org-mode, so clocking in etc works as you would expect.



;;; Code:

(require 'a)
(require 'all-the-icons)
(require 'azure)
(require 'dash)
(require 's)
(require 'svg-lib)
(require 'json)
(require 'org)
(require 'seq)
(require 'url-util)

(defgroup azure-devops nil
  "Azure-devops & org-mode working in symphony"
  :prefix "azure-devops-"
  :link '(url-link "https://github.com/hkjels/azure.el")
  :group 'azure
  :group 'tools)

;; Required information


(defcustom azure-devops-search-latest-atop t
  "Wether to order the search-results with the latest entry first or last."
  :group 'azure
  :type 'boolean)

(defcustom azure-devops-discussion-latest-atop nil
  "Wether to order the thread of discussion with the latest comment first or last."
  :group 'azure
  :type 'boolean)

(defcustom azure-devops-search-show-header t
  "Wether to show or hide the header in the search-results buffer."
  :group 'azure
  :type 'boolean)

(defcustom azure-devops-search-results-max 200
  "Maximum number of results returned when searching for work-items.
   Note that <b>200</b> is the maximum supported by Azure's API."
  :group 'azure
  :type 'natnum)

(defcustom azure-devops-search-buffer "*azure searching %P*"
  "Name of the buffer used to display search results.

   Note that you can add certain properties via formatting specifiers:
       %O - Organization
       %P - Project
       %T - Team"
  :group 'azure
  :type 'string)

(defcustom azure-devops-item-buffer "*azure - %t*"
  "Name of the buffer used to display a work-item.

   Note that you can add certain properties via formatting specifiers:
       %O - Organization
       %P - Project
       %T - Team
       %t - Item title
       %a - Item assignee"
  :group 'azure
  :type 'string)

(defconst azure-devops-work-item-todo-directive
  "#+TODO: NEW ACTIVE | RESOLVED CLOSED REMOVED\n"
  "Org TODO configuration used in individual work-item buffers.")

(defconst azure-devops-mapping-states
  '(("New" . "NEW")
    ("Active" . "ACTIVE")
    ("Resolved" . "RESOLVED")
    ("Closed" . "CLOSED")
    ("Removed" . "REMOVED"))
  "Map supported Azure work-item states to Org TODO keywords.")

;; Menus and bindings


(defvar azure-devops-search-mode-hook nil
  "Hook that's run when `azure-devops-search-mode` is turned on.")

(defvar azure-devops-search-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "RET") #'azure-devops-work-item)
    (define-key map (kbd "<double-mouse-1>") #'azure-devops-work-item)
    (let ((prefix-map (make-sparse-keymap)))
      (define-key prefix-map (kbd "SPC") 'azure-devops-search)
      (define-key prefix-map (kbd "k") 'azure-devops--keywords)
      (define-key prefix-map (kbd "t") (lambda () (interactive) (azure-devops--menu "type")))
      (define-key prefix-map (kbd "a") (lambda () (interactive) (azure-devops--menu "assignees")))
      (define-key prefix-map (kbd "s") (lambda () (interactive) (azure-devops--menu "state")))
      (define-key prefix-map (kbd "r") (lambda () (interactive) (azure-devops--menu "area")))
      (define-key prefix-map (kbd "i") (lambda () (interactive) (azure-devops--menu "iteration")))
      (define-key prefix-map (kbd "g") (lambda () (interactive) (azure-devops--menu "tags")))
      (define-key map (kbd azure-prefix-key) prefix-map))
    map)
  "Keymap used with the work-item search.")

(defvar azure-devops-work-item-menu
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used when visiting a work-item.")

;; Search

;; We have an interactive buffer where you can query for work-items
;; asynchronously.


(defvar azure-devops--skipped 0
  "When querying for work-items, this is the number of work-items that
will be skipped. Used internally for pagination.
Will be increments of `azure-devops-search-results-max`.")

(defvar azure-devops--query ""
  "Current work-item search query, retained while filtering or paging.")

;; Faces


(defface azure-devops-item-mine '((t :inherit font-lock-builtin-face))
  "Face used with work-items that are assigned to you."
  :group 'azure)

(defface azure-devops-item-active '((t :inherit bold))
  "Face used with active work-items."
  :group 'azure)

(defface azure-devops-item-new '((t :inherit bold))
  "Face used with new work-items."
  :group 'azure)

(defface azure-devops-item-closed '((t :inherit font-lock-comment-face))
  "Face used with closed work-items."
  :group 'azure)

(defface azure-devops-item-resolved '((t :inherit font-lock-comment-face))
  "Face used with resolved work-items."
  :group 'azure)

(defvar azure-devops-item-tags
  '(:background "#f3f8ff"
    :foreground "#2751e5"
    :stroke-color "#b9ceff"
    :font-size 8
    :font-weight 600
    :margin 1
    :stroke 1
    :radius 8)
  "Default style settings for tags.")

(defun azure-devops-face-by-state (state)
  (let ((state (downcase state)))
   (cond ((s-equals? state "new") 'azure-devops-item-new)
         ((s-equals? state "active") 'azure-devops-item-active)
         ((s-equals? state "closed") 'azure-devops-item-closed)
         ((s-equals? state "resolved") 'azure-devops-item-resolved))))

;; TODO Header [0/3]
;; - [ ] Improve alignment
;; - [ ] Add filtering functions
;; - [ ] Update upon changing filters

(defun azure-devops--build-filter-object ()
  "Builds a filter object based on the current filter settings."
  (let ((filter-object (make-hash-table :test 'equal)))
    (puthash "System.TeamProject" (list azure-project) filter-object)
    (when azure--keywords
      (puthash "System.Keywords" azure--keywords filter-object))
    (when azure--types
      (puthash "System.WorkItemType" azure--types filter-object))
    (when azure--assignees
      (puthash "System.AssignedTo" azure--assignees filter-object))
    (when azure--state
      (puthash "System.State" azure--state filter-object))
    (when azure--area
      (puthash "System.AreaPath" (list azure--area) filter-object))
    (when azure--iteration
      (puthash "System.IterationPath" (list azure--iteration) filter-object))
    (when azure--tags
      (puthash "System.Tags" (list azure--tags) filter-object))
    filter-object))

(defun azure-devops--clear-all-filters ()
  (interactive)
  (setq azure--types nil
	azure--keywords nil
        azure--assignees nil
        azure--state nil
        azure--area nil
        azure--iteration nil
        azure--tags nil)
  (azure-devops--search))

(defun azure-devops--test-output ()
  (interactive)
  (azure-log this-command "Test output"))

(defun azure-devops--define-mouse-key (command &optional args)
  "General mouse handler that takes a COMMAND and optionally ARGS."
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1]
                (lambda (click)
                  (interactive "e")
                  (mouse-select-window click)
                  (apply command args)))
    map))

(defun azure-devops--keywords ()
  (interactive)
  (let ((keywords (read-string "Filter by keyword: ")))
    (setq azure--keywords keywords)
    (azure-devops--search)))

(defun azure-devops--parse-work-item-types (data)
  "Parse the work item types DATA from Azure DevOps API."
  (mapcar (lambda (item) (cdr (assoc 'name item))) data))

(defun azure-devops--fetch-work-item-types (callback)
  "Fetch available work item types.

See URL: https://learn.microsoft.com/en-us/rest/api/azure/devops/wit/work-item-types/list?view=azure-devops-rest-7.1
for more information."
  (azure-get "https://dev.azure.com/{organization}/{project}/_apis/wit/workitemtypes"
	     (cl-function
	      (lambda (&key data &allow-other-keys)
                (let ((types (azure-devops--parse-work-item-types (cdr (assoc 'value data))))
		      (this-command "azure-devops--fetch-work-item-types"))
		  (funcall callback types)
		  (azure-log this-command "%S" types))))
	     '(("api-version" . "7.1-preview.2"))))

(defun azure-devops--get-available-types ()
  "Get available work item types and allow the user to select multiple."
  (interactive)
  (unless azure--available-types
    (azure-devops--fetch-work-item-types
     (lambda (types)
       (setq azure--available-types types))))
  (setq azure--types (completing-read-multiple "Select work item types: " azure--available-types))
  (azure-devops--search))

(defun azure-devops--get-available-team-members ()
  "Fetch team members, then select assignees by their full identities.

Use semicolons to separate multiple selections because Azure display
names may themselves contain commas."
  (interactive)
  (azure--team-members
   (lambda (members)
     ;; Refresh on every invocation so a project or team change cannot
     ;; leave this picker using identities from an earlier selection.
     (setq azure--available-team-members members)
     (let* ((crm-separator "[ \t]*;[ \t]*")
            (identities (mapcar #'car members))
            (selected
             (completing-read-multiple
              "Select assignees (separate multiple with ;): "
              identities nil t)))
       (setq azure--assignees selected)
       (azure-devops--search)))))

(defun azure-devops--menu (type)
  "Open a dynamic menu based on the TYPE of the header."
  (interactive)
  (cond
   ((string= type "type") (azure-devops--get-available-types))
   ((string= type "assignees") (azure-devops--get-available-team-members))
   ;; ((string= type "state") (azure-devops--get-available-states))
   ;; ((string= type "area") (azure-devops--get-available-areas))
   ;; ((string= type "iteration") (azure-devops--get-available-iterations))
   ;; ((string= type "tags") (azure-devops--get-available-tags))
   (t (error "Unknown menu type: %s" type))))

(defun azure-devops--search-header-types ()
  "Tap the types-name in the header-line to change it."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--menu '("type"))))
    `(:propertize ,(truncate-string-to-width (s-join ", " (or azure--types '("Types"))) 15 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Filter by type"
                  keymap ,map)))

(defun azure-devops--search-header-assignee ()
  "Tap the assignee-name in the header-line to change it."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--menu '("assignees"))))
    `(:propertize ,(truncate-string-to-width (s-join ", " (or azure--assignees '("Assigned to"))) 15 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Filter by assignee"
                  keymap ,map)))

(defun azure-devops--search-header-state ()
  "Tap the state-name in the header-line to change it."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--menu '("state"))))
    `(:propertize ,(truncate-string-to-width (s-join ", " (or azure--state '("State"))) 15 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Filter by state"
                  keymap ,map)))

(defun azure-devops--search-header-area ()
  "Tap the area-name in the header-line to change it."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--menu '("area"))))
    `(:propertize ,(truncate-string-to-width (s-join ", " (or azure--area '("Area"))) 15 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Filter by area"
                  keymap ,map)))

(defun azure-devops--search-header-iteration ()
  "Tap the iteration-name in the header-line to change it."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--menu '("iteration"))))
    `(:propertize ,(truncate-string-to-width (s-join ", " (or azure--iteration '("Iteration"))) 15 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Filter by iteration"
                  keymap ,map)))

(defun azure-devops--search-header-tags ()
  "Tap the tags-name in the header-line to change it."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--menu '("tags"))))
    `(:propertize ,(truncate-string-to-width (s-join ", " (or azure--tags '("Tags"))) 15 nil 32 "…")
                  mouse-face header-line-highlight
                  help-echo "Filter by tags"
                  keymap ,map)))

(defun azure-devops--search-header-clear ()
  "Button to clear all filters."
  (let ((map (azure-devops--define-mouse-key 'azure-devops--clear-all-filters)))
    `(:propertize ,(all-the-icons-faicon "times-circle")
		  mouse-face header-line-highlight
		  help-echo "Clear all filters"
		  keymap ,map)))

(defun azure-devops--search-header-line ()
  "Header-line used with the search-buffer to enable various filtering."
  (let ((space "  "))
    (setq-local header-line-format
		(list
		 (azure-devops--search-header-types) space
		 (azure-devops--search-header-assignee) space
		 (azure-devops--search-header-state) space
		 (azure-devops--search-header-area) space
		 (azure-devops--search-header-iteration) space
		 (azure-devops--search-header-tags) space
		 (azure-devops--search-header-clear)))))

;; [[https://docs.microsoft.com/en-us/rest/api/azure/devops/search/work-item-search-results/fetch-work-item-search-results][Work Item Search Results]]


(defun azure-devops-search-selected-id ()
  (let ((buf (get-buffer (azure-devops--buffer-name azure-devops-search-buffer))))
    (with-current-buffer buf
      (let ((line (buffer-substring-no-properties
		   (line-beginning-position)
		   (line-end-position))))
	(->> (s-collapse-whitespace line)
             (s-match "^[^0-9]*\\([0-9]+\\)")
             (cl-first)
             (s-trim)
             (string-to-number))))))

(defun azure-devops--search (&optional text skip)
  "Query Azure's API for work items.

See URL `https://docs.microsoft.com/en-us/rest/api/azure/devops/search/work-item-search-results/fetch-work-item-search-results'
for more information."
  (let ((url "https://almsearch.dev.azure.com/{organization}/{project}/_apis/search/workitemsearchresults")
        (top (math-min (math-max 0 azure-devops-search-results-max) 200))
        (skip (or skip 0))
        (text (if (null text) azure-devops--query text))
	(filters (azure-devops--build-filter-object)))
    (azure-post url
                (cl-function
                 (lambda (&key data &allow-other-keys)
                   (let* ((work-items (mapcar
                                       (lambda (item)
                                         (mapcar 'cdr (cdr (assoc 'fields item))))
                                       (cdr (assoc 'results data))))
                          (work-items (sort work-items
                                            (lambda (a b)
                                              (not (s-less? (nth 7 a) (nth 7 b))))))
                          (work-items (if azure-devops-search-latest-atop work-items (reverse work-items))))
                     (when (not (eq azure-devops--work-items work-items))
                       (progn
                         (setq azure-devops--work-items work-items)
                         (azure-devops--update-search-buffer))))))
                `(("searchText" . ,(if (and text (not (string= "" text))) text "NOT null"))
                  ("$orderBy" . ((("field" . "system.id") 
                                  ("sortOrder" . "DESC"))))
                  ("$skip" . ,skip)
                  ("$top" . ,top)
		  ;; `azure-req' JSON-encodes the whole request once.
                  ("filters" . ,filters)
                  ("includeFacets" . t))
                '(("api-version" . "7.1-preview.1")))))

;; TODO Results buffer [3/8]

;; - [X] Make sure font-locking only spans one line at a time
;; - [ ] Color read items differently
;; - [ ] Color assigned items differently
;; - [ ] Use mode-menu in mode-line
;; - [X] Add item-type icon (bug, user-story, etc)
;; - [X] Replace tags using svg-lib
;; - [ ] Apply a fringe indicator if an item was updated after viewing it
;; - [ ] Use transient to enable more powerful search, filtering, creation, etc

;; When doing a search (~azure-devops-search~), we validate the configuration
;; first via ~azure-init~.  The rest is handled interactively from inside
;; the search results buffer.

(defun azure-devops--buffer-name (buffer-name)
  "Get the formatted/compiled BUFFER-NAME."
  (s-replace-all `(("%O" . ,azure-organization)
                   ("%P" . ,azure-project)
                   ("%T" . ,azure-team))
                 buffer-name))

(defvar azure-devops--work-items '()
  "Work-items currently being listed.")

(defun azure-devops-search-selected ()
  "Return the currently selected work-item from the search results list."
  (let* ((item-num (- (line-number-at-pos (point)) 1))
         (work-item (nth item-num azure-devops--work-items)))
    work-item))

(defun azure-devops--setup-search-buffer ()
  "Setup of the buffer that holds our search-results.
\\{azure-devops-search-mode-map}"
  (let ((buf (get-buffer-create (azure-devops--buffer-name azure-devops-search-buffer))))
    (switch-to-buffer buf)
    (kill-all-local-variables)
    (hack-dir-local-variables)
    (hack-local-variables-apply)
    (use-local-map azure-devops-search-mode-map)
    (read-only-mode t)
    (buffer-disable-undo)
    (setq-local truncate-lines t
                line-move-visual t
                show-trailing-whitespace nil)))

(defun azure-devops--update-search-buffer ()
  "Update the search buffer with the current work items."
  (let ((buf (get-buffer (azure-devops--buffer-name azure-devops-search-buffer)))
	(this-command "azure-devops--update-search-buffer"))
    (azure-log this-command "Work items: %S" azure-devops--work-items)
    (with-current-buffer buf
      (hl-line-mode t)
      (when azure-devops-search-show-header
	(azure-devops--search-header-line))
      (save-excursion
	(setq inhibit-read-only t)
	(when (equal 0 azure-devops--skipped)
	  (delete-region (point-min) (point-max)))
	(goto-line azure-devops--skipped)
	(beginning-of-line (if (> azure-devops--skipped 0) 1 0))
	(dolist (item azure-devops--work-items)
	  (pcase-let
	      ((`(,id ,type ,title ,assignee ,state ,tags . ,_) item))
	    (let* ((width (max 50 (- (window-width) 60 (string-width "\t\t\t\t"))))
		   (title (truncate-string-to-width (s-collapse-whitespace title) width nil 32 "…"))
		   (face (if (string= assignee azure--user) 'azure-devops-item-mine (azure-devops-face-by-state state)))
		   (item-type (cond ((s-equals? type "Bug") (all-the-icons-material "bug_report" :face face))
				    ((s-equals? type "User Story") (all-the-icons-octicon "book" :face face))
				    ((s-equals? type "Feature") (all-the-icons-octicon "rocket" :face face))
				    ((s-equals? type "Task") (all-the-icons-octicon "checklist" :face face))
				    (t ""))))
	      (insert (propertize (format "%-10s\t%-8s" id state) 'font-lock-face face))
	      (insert (propertize (format "\t%s " item-type) 'help-echo (format " %s " type)))
	      (insert (propertize (format "%s\t" title) 'font-lock-face face))
	      (when (s-present? tags)
		(dolist (tag (s-split ";" tags))
		  (insert-image (apply #'svg-lib-tag tag
				       '(svg-lib-style-compute-default)
				       azure-devops-item-tags))))
	      (insert (propertize "\n" 'font-lock-face face)))))
	(setq inhibit-read-only nil)))))

(defun azure-devops-search-skip ()
  (when (and (s-starts-with? (buffer-name (current-buffer)) "*azure search")
             (= (point) (point-max)))
    (let ((skip (+ azure-devops--skipped azure-devops-search-results-max)))
      (azure-log this-command "Reached the end of the search-buffer")
      (setq azure-devops--skipped skip)
      (azure-devops--search azure-devops--query skip))))

(add-hook 'post-command-hook 'azure-devops-search-skip)

;;;###autoload
(define-derived-mode azure-devops-search-mode special-mode "azure-devops-search"
  "Major-mode to search for work-items.

\\{azure-devops-search-mode-map}"
  :group 'azure
  :after-hook azure-devops-search-mode-hook
  :syntax-table nil
  :abbrev-table nil
  (add-hook 'window-configuration-change-hook 'azure-devops--update-search-buffer nil 'local))

;;;###autoload
(defun azure-devops-search (query)
  "Opens a dedicated search-buffer for work-items in azure devops."
  (interactive (list (read-string "Enter search query (leave empty to return all work items): ")))
  (unless (azure--valid-p)
    (user-error "You need to run `azure-init` first!"))
  (azure-devops--setup-search-buffer)
  (azure-devops-search-mode)
  (azure--set-user)
  (setq azure-devops--query query)
  (azure-devops--search query)
  (run-mode-hooks 'azure-devops-search-mode-hook))

;; Comments


(defun azure-devops--comments (id)
  ""
  (promise-new
   (lambda (resolve _reject)
     (let ((url (format "https://dev.azure.com/{organization}/{project}/_apis/wit/workItems/%d/comments" id)))
       (azure-get url
                  (cl-function
                   (lambda (&key data &allow-other-keys)
                     (let ((comments (cdr (assoc 'comments data)))
                           (this-command "azure-devops--comments"))
                       (funcall resolve comments)
                       (azure-log this-command "%S" comments))))
                  '(("api-version" . "7.1-preview.3")))))))

;; Work items


(defun azure-devops--item-buffer (title assignee)
  "Returns the compiled name of a work-item buffer."
  (s-replace-all `(("%O" . ,azure-organization)
                   ("%P" . ,azure-project)
                   ("%T" . ,azure-team)
                   ("%t" . ,title)
                   ("%a" . ,assignee))
                 azure-devops-item-buffer))

;; TODO Work Item Buffer [0/3]

;; - [ ] Enable editing
;; - [ ] Make sure links can be followed within azure-devops.el scope
;; - [ ] Use view-mode until changes are synchronized


(defun azure-devops-work-item-file (id)
  "Expanded file-path of the work-item prefixed with ID."
  (car
   (file-expand-wildcards
    (expand-file-name (format "%d-*.org" id) azure-cache-directory))))

(defun azure-devops--create-or-flush-work-item-buffer (id)
  "Open the file associated with the work-item with ID and update it's content.

   If a file does not exist, a new one will be created."
  (promise-new
   (lambda (resolve _reject)
     (let ((logbook-p nil)
           (check-point (point-min))
           (this-command "azure-devops--create-or-flush-work-item-buffer"))
       (when (eq (azure-devops-work-item-file id) nil)
         (let* ((new-name (format "%d-Not-yet-updated.org" id))
                (buf (generate-new-buffer new-name)))
           (azure-log this-command "Creating a new work-item file named: %S" new-name)
           (save-excursion
             (with-current-buffer buf
               (org-mode)
               (insert "\n\n* Personal Notes\n")
               (write-file (expand-file-name new-name azure-cache-directory))))))
       (azure-log this-command "Open file on disk, regardless if it’s new or old")
       (find-file (azure-devops-work-item-file id))
       (with-current-buffer (current-buffer)
         (goto-char check-point)
         (save-excursion
           (while (re-search-forward ":logbook:" nil 'noerror)
             (azure-log this-command "Logbook entry exists, delete everything before the entry")
             (delete-region (point) (match-beginning 0))
             (setq logbook-p t)))
         (when logbook-p
           (azure-log this-command "Move pointer to after the logbook entry")
           (while (re-search-forward ":logbook:.+:end:" nil)
             (setq check-point (match-end 0))
             (goto-char check-point)))
         ;; Links are derived from Azure and regenerated on every refresh.
         ;; Remove the old final section while preserving Personal Notes.
         (save-excursion
           (goto-char (point-min))
           (when (re-search-forward "^\\* Personal Notes[ \t]*$" nil 'noerror)
             (when (re-search-forward "^\\* Links[ \t]*$" nil 'noerror)
               (delete-region (line-beginning-position) (point-max)))))
         (save-excursion
          (while (re-search-forward "^\\* Personal Notes[ \t]*$" nil 'noerror)
            (when (length> (buffer-substring-no-properties check-point (- (match-beginning 0) 1)) 1)
              (azure-log this-command "Delete everything from the pointer (line %d) to the personal notes section (line %d)"
                         (line-number-at-pos check-point)
                         (line-number-at-pos (- (match-beginning 0) 1)))
              (delete-region check-point (- (match-beginning 0) 1)))))
         (azure-log this-command "Return the work-item buffer: %S" (buffer-name (current-buffer)))
         (funcall resolve (buffer-name (current-buffer))))))))

(defun azure-devops--work-item-properties (work-item)
  "Creates a properties drawer for essential WORK-ITEM information."
  (let* ((fields (cdr (assoc 'fields work-item)))
         (id (cdr (assoc 'id work-item)))
         (rev (cdr (assoc 'rev work-item)))
         (state (cdr (assoc 'System.State fields)))
         (created (cdr (assoc 'System.CreatedDate fields)))
         (by (cdr (assoc 'displayName
                         (cdr (assoc 'System.CreatedBy fields))))))
    (azure-log this-command "Adding properties for: %d" id)
    (format ":properties:\n:id: %d\n:rev: %d\n:state: %s\n:created: %s\n:created-by: %s\n:end:\n" id rev state created by)))

(defun azure-devops--work-item-title (work-item)
  "Format WORK-ITEM's title and state as an Org heading."
  (let* ((fields (cdr (assoc 'fields work-item)))
         (azure-state (cdr (assoc 'System.State fields)))
         (todo-keyword (cdr (assoc azure-state azure-devops-mapping-states)))
         (title (cdr (assoc 'System.Title fields))))
    (azure-log this-command "Adding title: %s" title)
    (format "* %s%s\n"
            (if todo-keyword (concat todo-keyword " ") "")
            title)))

(defun azure-devops--work-item-type (work-item)
  "Return an icon that represents the type of the WORK-ITEM."
  (let* ((fields (cdr (assoc 'fields work-item)))
         (id (cdr (assoc 'id work-item)))
         (item-type (downcase (cdr (assoc 'System.WorkItemType fields)))))
    (azure-log this-command "Adding work-item type for: %d" id)
    (cond ((s-equals? item-type "bug") (propertize (all-the-icons-material "bug_report")
                                                   'help-echo `item-type))
          ((s-equals? item-type "user story") (propertize (all-the-icons-octicon "book")
                                                          'help-echo `item-type))
          (t ""))))

(defun azure-devops--work-item-content (work-item)
  "Return the body (description and repro steps) of WORK-ITEM."
  (let* ((fields (cdr (assoc 'fields work-item)))
         (description (cdr (assoc 'System.Description fields)))
         (repro (cdr (assoc 'Microsoft.VSTS.TCM.ReproSteps fields))))
    (s-join
     "\n\n"
     (delq nil
           (list
            (when description
              (azure--html-to-org description))
            (when repro
              (azure--html-to-org repro)))))))

(defun azure-devops--work-item-comments (comments)
  "Format COMMENTS into a discussions section."
  (let ((comments (if azure-devops-discussion-latest-atop comments (reverse comments)))
        (template (s-join "\n" [":properties:"
                                ":id: %d"
                                ":created: %s"
                                ":created-by: %s"
                                ":end:"
                                "%s"
                                ""])))
    (azure-log this-command "Discussion (%d): %S" (length comments) comments)
    (format "\n\n* Discussion (%d)\n\n%s" (length comments) 
            (s-join "\n" (mapcar
                          (lambda (comment)
                            (let ((id (cdr (assoc 'id comment)))
                                  (text (s-trim (azure--html-to-org (cdr (assoc 'text comment)))))
                                  (by (cdr (assoc 'displayName (cdr (assoc 'createdBy comment)))))
                                  (created (cdr (assoc 'createdDate comment))))
                              (format template id created by text)))
                          comments)))))

(defun azure-devops--relation-work-item-id (relation)
  "Return the work-item ID addressed by RELATION, or nil.

Relations to commits, attachments, hyperlinks, and other non-work-item
resources are deliberately ignored."
  (let ((url (cdr (assoc 'url relation))))
    (when (and (stringp url)
               (string-match
                "/work[Ii]tems/\\([0-9]+\\)\\(?:[?#].*\\)?\\'" url))
      (string-to-number (match-string 1 url)))))

(defun azure-devops--related-work-items (work-item)
  "Return a promise for the work-item relations of WORK-ITEM.

Each result is a pair whose car is the relation and whose cadr is the
linked work item.  Linked work items are fetched so their titles can be
shown."
  (let ((relations
         (seq-filter #'azure-devops--relation-work-item-id
                     (cdr (assoc 'relations work-item)))))
    (if (null relations)
        (promise-resolve nil)
      (promise-then
       (promise-all
        (vconcat
         (mapcar
          (lambda (relation)
            (promise-then
             (azure-devops--work-item-get
              (azure-devops--relation-work-item-id relation))
             (lambda (related) (list relation related))))
          relations)))
       (lambda (related) (append related nil))))))

(defun azure-devops--relation-link-type (entry)
  "Return the displayed Azure relation type for related work-item ENTRY."
  (let* ((relation (car entry))
         (attributes (cdr (assoc 'attributes relation))))
    (or (cdr (assoc 'name attributes))
        (cdr (assoc 'rel relation))
        "Related")))

(defun azure-devops--related-work-item-title (entry)
  "Return a single-line title for related work-item ENTRY."
  (let* ((work-item (cadr entry))
         (id (cdr (assoc 'id work-item)))
         (fields (cdr (assoc 'fields work-item))))
    (replace-regexp-in-string
     "[\n\r]+" " "
     (or (cdr (assoc 'System.Title fields))
         (format "Work item %s" id)))))

(defun azure-devops--relation-sort-less-p (left right)
  "Return non-nil when related work-item LEFT should precede RIGHT.

Parents come first, children second, and other relation types follow
alphabetically.  Entries of the same type are sorted by task title."
  (let* ((left-type (azure-devops--relation-link-type left))
         (right-type (azure-devops--relation-link-type right))
         (left-type-folded (downcase left-type))
         (right-type-folded (downcase right-type))
         (left-rank (cond ((string= left-type-folded "parent") 0)
                          ((string= left-type-folded "child") 1)
                          (t 2)))
         (right-rank (cond ((string= right-type-folded "parent") 0)
                           ((string= right-type-folded "child") 1)
                           (t 2))))
    (or (< left-rank right-rank)
        (and (= left-rank right-rank)
             (or (string-lessp left-type-folded right-type-folded)
                 (and (string= left-type-folded right-type-folded)
                      (string-lessp
                       (downcase (azure-devops--related-work-item-title left))
                       (downcase (azure-devops--related-work-item-title right)))))))))

(defun azure-devops--work-item-relations (related-work-items)
  "Format RELATED-WORK-ITEMS as a sorted Org description list."
  (if (null related-work-items)
      ""
    (concat
     (mapconcat
      (lambda (entry)
        (let* ((work-item (cadr entry))
               (link-type (azure-devops--relation-link-type entry))
               (id (cdr (assoc 'id work-item)))
               (title (azure-devops--related-work-item-title entry)))
          (format "- %s :: %s"
                  link-type
                  (org-link-make-string
                   (format "azure-work-item:%s" id) title))))
      (sort (copy-sequence related-work-items)
            #'azure-devops--relation-sort-less-p)
      "\n")
     "\n\n")))

(defun azure-devops--follow-work-item-link (path _argument)
  "Open the Azure work item identified by link PATH."
  (unless (string-match-p "\\`[0-9]+\\'" path)
    (user-error "Invalid Azure work-item ID: %s" path))
  (azure-devops-work-item (string-to-number path)))

(defun azure-devops--search-result-field (fields name)
  "Return from FIELDS the value whose field name equals NAME.

Comparison is case-insensitive because Azure's Search API has used
multiple capitalizations of its field names."
  (cdr
   (seq-find
    (lambda (field)
      (string-equal (downcase (format "%s" (car field)))
                    (downcase name)))
    fields)))

(defun azure-devops--identity-display-name (identity)
  "Return a concise display name for Azure IDENTITY."
  (cond
   ((stringp identity) identity)
   ((listp identity)
    (or (cdr (assoc 'displayName identity))
        (cdr (assoc 'uniqueName identity))
        ""))
   (t "")))

(defun azure-devops--link-search-results (data)
  "Extract completion data from Azure Search response DATA.

Each result has the form (ID TITLE STATE ASSIGNEE)."
  (delq
   nil
   (mapcar
    (lambda (item)
      (let* ((fields (cdr (assoc 'fields item)))
             (id (or (azure-devops--search-result-field fields "System.Id")
                     (cdr (assoc 'id item))
                     (car (mapcar #'cdr fields))))
             (title (or (azure-devops--search-result-field
                         fields "System.Title")
                        (nth 2 (mapcar #'cdr fields))))
             (state (azure-devops--search-result-field
                     fields "System.State"))
             (assignee (azure-devops--identity-display-name
                        (azure-devops--search-result-field
                         fields "System.AssignedTo"))))
        (when id
          (list (format "%s" id)
                (replace-regexp-in-string
                 "[\n\r]+" " "
                 (or title (format "Work item %s" id)))
                (or state "")
                assignee))))
    (cdr (assoc 'results data)))))

(defun azure-devops--link-search-text (query)
  "Return Azure Search text for link-completion QUERY.

Plain words become prefix searches, so, for example, `Ontol' matches
both `ontology' and `Ontology'.  Azure Search performs the matching
case-insensitively.  Queries using Azure's advanced syntax are left
unchanged."
  (let ((query (string-trim query)))
    (cond
     ((string-empty-p query) "NOT null")
     ((string-match-p
       "\\`[[:alnum:]_-]+\\(?:[[:space:]]+[[:alnum:]_-]+\\)*\\'"
       query)
      (mapconcat (lambda (word) (concat word "*"))
                 (split-string query nil t)
                 " "))
     (t query))))

(defun azure-devops--search-work-items-for-link (query success)
  "Search for QUERY and call SUCCESS with completion item lists.

Plain words in QUERY use case-insensitive prefix matching."
  (let ((url "https://almsearch.dev.azure.com/{organization}/{project}/_apis/search/workitemsearchresults")
        (filters (make-hash-table :test 'equal)))
    (puthash "System.TeamProject" (list azure-project) filters)
    (azure-post
     url
     (cl-function
      (lambda (&key data &allow-other-keys)
        (funcall success (azure-devops--link-search-results data))))
     `(("searchText" . ,(azure-devops--link-search-text query))
       ("$orderBy" . (( ("field" . "system.id")
                         ("sortOrder" . "DESC"))))
       ("$skip" . 0)
       ("$top" . ,azure-devops-search-results-max)
       ("filters" . ,filters)
       ("includeFacets" . t))
     '(("api-version" . "7.1-preview.1")))))

(defvar azure-devops--work-item-link-titles (make-hash-table :test #'equal)
  "Titles remembered while completing `azure-work-item' links.")

(defun azure-devops--work-item-candidate (item)
  "Return a completion candidate for work ITEM.

ITEM has the form (ID TITLE STATE ASSIGNEE)."
  (let ((id (nth 0 item))
        (title (nth 1 item))
        (state (nth 2 item))
        (assignee (nth 3 item)))
    (format "%-10s  %-28s  %s  (#%s)"
            (or state "")
            (or assignee "")
            title
            id)))

(defun azure-devops--select-work-item (results)
  "Prompt for and return one work item from RESULTS.

Each element of RESULTS has the form (ID TITLE STATE ASSIGNEE)."
  (unless results
    (user-error "No matching Azure work items"))
  (let* ((candidates
          (mapcar
           (lambda (item)
             (cons (azure-devops--work-item-candidate item) item))
           results))
         (completion-extra-properties
          '(:category azure-devops-work-item))
         (choice (completing-read
                  "Work item (state, assignee, title): "
                  candidates nil t))
         (item (cdr (assoc choice candidates))))
    (or item (user-error "No work item selected"))))

(defun azure-devops--read-work-item-id ()
  "Interactively select and return an Azure work-item ID.

Retrieve the current work-item list, then let the completion interface
perform incremental narrowing."
  (unless (azure--valid-p)
    (user-error "You need to run `azure-init` first!"))
  (let ((item (azure-devops--select-work-item
               (azure-devops--wait-for-link-search ""))))
    (string-to-number (car item))))

(defun azure-devops--insert-selected-work-item-link (marker results)
  "At MARKER, insert an Azure work-item link selected from RESULTS."
  (let ((item (azure-devops--select-work-item results)))
    (unless (and (markerp marker) (marker-buffer marker))
      (user-error "The buffer for the Azure link no longer exists"))
    (with-current-buffer (marker-buffer marker)
      (goto-char marker)
      (org-insert-link nil
                       (format "azure-work-item:%s" (car item))
                       (cadr item)))))

(defun azure-devops--wait-for-link-search (query)
  "Synchronously return Azure work items matching QUERY.

Org's link completion protocol requires a string return value, so its
otherwise asynchronous Azure request must finish before completion returns."
  (let (done results response)
    (setq response
          (azure-devops--search-work-items-for-link
           query
           (lambda (items)
             (setq results items
                   done t))))
    (while (and (not done)
                (not (and (request-response-p response)
                          (request-response-done-p response))))
      (accept-process-output nil 0.05))
    (unless done
      (let ((reason (and (request-response-p response)
                         (request-response-error-thrown response))))
        (error "Azure work-item search failed%s"
               (if reason (format ": %s" reason) ""))))
    results))

(defun azure-devops--complete-work-item-link (&optional _prefix)
  "Return an `azure-work-item' link selected with Azure search.

This function implements Org's synchronous `:complete' protocol."
  (unless (azure--valid-p)
    (user-error "You need to run `azure-init` first!"))
  (let* ((query (read-string
                 "Search Azure work items (empty means all): "))
         (item (azure-devops--select-work-item
                (azure-devops--wait-for-link-search query)))
         (location (format "azure-work-item:%s" (car item))))
    (puthash location (cadr item) azure-devops--work-item-link-titles)
    location))

(defun azure-devops--work-item-link-description (location _description)
  "Return the title remembered for Azure work-item LOCATION."
  (gethash location azure-devops--work-item-link-titles))

;;;###autoload
(defun azure-devops-insert-work-item-link (&optional _prefix)
  "Search Azure DevOps and asynchronously insert a selected work-item link."
  (interactive "P")
  (unless (azure--valid-p)
    (user-error "You need to run `azure-init` first!"))
  (let ((query (read-string
                "Search Azure work items (empty means all): "))
        (marker (copy-marker (point) t)))
    (azure-devops--search-work-items-for-link
     query
     (lambda (results)
       (azure-devops--insert-selected-work-item-link marker results)))))

(org-link-set-parameters "azure-work-item"
                         :follow #'azure-devops--follow-work-item-link
                         :complete #'azure-devops--complete-work-item-link
                         :insert-description
                         #'azure-devops--work-item-link-description
                         :help-echo "Open this work item in Emacs")

(defun azure-devops--first-heading ()
  "Return the first Org headline element in the current buffer."
  (or (org-element-map
          (org-element-parse-buffer) 'headline #'identity nil t)
      (user-error "This buffer has no Org heading for an Azure work item")))

(defun azure-devops--first-heading-title ()
  "Return the title of the first Org heading in the current buffer.

Org TODO keywords, priorities, and tags are not part of the returned title."
  (let ((title (string-trim
                (or (org-element-property
                     :raw-value (azure-devops--first-heading))
                    ""))))
    (when (string-empty-p title)
      (user-error "The Azure work-item title cannot be empty"))
    title))

(defun azure-devops--first-heading-state ()
  "Return the Azure state selected on the first heading, or nil.

A heading without a TODO keyword deliberately leaves the Azure state
unchanged.  Signal a user error if a TODO keyword is present but is not one
of the supported Azure state keywords."
  (let ((keyword (org-element-property
                  :todo-keyword (azure-devops--first-heading))))
    (when keyword
      (or (car (rassoc keyword azure-devops-mapping-states))
          (user-error "Cannot push unsupported Azure state keyword %S"
                      keyword)))))

(defun azure-devops--first-heading-description ()
  "Return the Org body of the first heading in the current buffer.

The full contents of that heading, including any child headings, form the
work-item description.  Later sibling headings such as Discussion, Personal
Notes, and Links are excluded by Org's headline boundaries."
  (let ((headline (azure-devops--first-heading)))
    (if-let* ((begin (org-element-property :contents-begin headline))
              (end (org-element-property :contents-end headline)))
        (string-trim (buffer-substring-no-properties begin end))
      "")))

(defun azure-devops--document-property (name)
  "Return the first Org node property named NAME in the current buffer."
  (org-element-map
      (org-element-parse-buffer) 'node-property
    (lambda (property)
      (when (string-equal
             (downcase (org-element-property :key property))
             (downcase name))
        (org-element-property :value property)))
    nil t))

(defun azure-devops--set-document-property (name value)
  "Set the first Org node property named NAME to VALUE.

The property is found through Org's element API.  Signal an error when it is
not present, since Azure work-item buffers are expected to contain it."
  (let ((property
         (org-element-map
             (org-element-parse-buffer) 'node-property
           (lambda (candidate)
             (when (string-equal
                    (downcase (org-element-property :key candidate))
                    (downcase name))
               candidate))
           nil t)))
    (unless property
      (error "Azure work-item property %s is missing" name))
    (let ((inhibit-read-only t)
          (key (org-element-property :key property)))
      (save-excursion
        (goto-char (org-element-property :begin property))
        (delete-region (line-beginning-position) (line-end-position))
        (insert (format ":%s: %s" key value))))))

;;;###autoload
(defun azure-devops-update-work-item-description ()
  "Push the first Org heading's title and body to Azure.

The heading title becomes the Azure work-item title.  Its body becomes the
work-item description.  A supported TODO keyword becomes the Azure state.
Org priorities and tags are excluded from the title.  When the heading has
no TODO keyword, leave the Azure state unchanged.

The work-item ID and revision are read from the document property drawer.
The revision is tested by Azure before updating, preventing an unnoticed
write over a newer server revision.  On success, update the local revision
and, when pushed, state properties.  Other fields, comments, personal notes,
and links are untouched."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command must be run in an Org work-item buffer"))
  (unless (azure--valid-p)
    (user-error "You need to run `azure-init` first!"))
  (let* ((id-text (azure-devops--document-property "id"))
         (revision-text (azure-devops--document-property "rev"))
         (id (and id-text (string-to-number id-text)))
         (revision (and revision-text (string-to-number revision-text))))
    (unless (and id-text (> id 0))
      (user-error "This buffer has no valid Azure work-item ID"))
    (unless (and revision-text (> revision 0))
      (user-error "This buffer has no valid Azure work-item revision"))
    (let* ((title (azure-devops--first-heading-title))
           (state (azure-devops--first-heading-state))
           (description (azure-devops--first-heading-description))
           (html (azure--org-to-html description))
           (source-buffer (current-buffer))
           (url (format "https://dev.azure.com/{organization}/{project}/_apis/wit/workitems/%d"
                        id))
           (patch
            `(( ("op" . "test")
                ("path" . "/rev")
                ("value" . ,revision))
              ( ("op" . "add")
                ("path" . "/fields/System.Title")
                ("value" . ,title))
              ( ("op" . "add")
                ("path" . "/fields/System.Description")
                ("value" . ,html)))))
      (when state
        (setq patch
              (append patch
                      `(( ("op" . "add")
                          ("path" . "/fields/System.State")
                          ("value" . ,state))))))
      (azure-req
       "PATCH" url
       (cl-function
        (lambda (&key data &allow-other-keys)
          (let ((new-revision (cdr (assoc 'rev data))))
            (when (and new-revision (buffer-live-p source-buffer))
              (with-current-buffer source-buffer
                (azure-devops--set-document-property "rev" new-revision)
                (when state
                  (azure-devops--set-document-property "state" state))))
            (message "Updated Azure work item %d title, description%s%s"
                     id
                     (if state (format ", and state (%s)" state) "")
                     (if new-revision
                         (format " (revision %s)" new-revision)
                       "")))))
       '(("api-version" . "7.1"))
       patch
       '(("Content-Type" . "application/json-patch+json"))))))

;; We retrieve all the information needed first and if that succeeds,
;; we replace everything in our local copy of the issue with what we
;; retrieved. Only clocking and personal notes are persisted from the
;; local copy.  The final Links section is regenerated from Azure.
(async-defun azure-devops--update-work-item-buffer (id)  
  "Update the work-item buffer for the work-item with ID."
  (let* ((work-item (await (azure-devops--work-item-get id)))
         (comments (await (azure-devops--comments id)))
         (related-work-items (await (azure-devops--related-work-items work-item)))
         (buf (await (azure-devops--create-or-flush-work-item-buffer id)))
         (fields (cdr (assoc 'fields work-item)))
         (filename (format "%s.org" (s-dashed-words (cdr (assoc 'System.Title fields)))))
         (logbook-p nil)
         (this-command "azure-devops--update-work-item-buffer"))
    (with-current-buffer buf
      (goto-char (point-min))
      ;; A document property drawer must precede file-level keywords for Org to
      ;; recognize it as a property drawer.
      (insert (azure-devops--work-item-properties work-item))
      (insert azure-devops-work-item-todo-directive "\n")
      (insert (azure-devops--work-item-type work-item) "\n")
      (insert (azure-devops--work-item-title work-item))
      (save-excursion
        (while (re-search-forward ":logbook:" nil 'noerror)
          (azure-log this-command "Logbook entry was found!")
          (setq logbook-p t)))
      (when logbook-p
        (while (re-search-forward ":end:" nil)
          (azure-log this-command "Logbook entry was closed!")
          (goto-char (match-end 0))))
      (insert (azure-devops--work-item-content work-item))
      (insert (azure-devops--work-item-comments comments))
      ;; Keep generated navigation outside the prospective synchronized
      ;; work-item body and below the locally persisted Personal Notes.
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n" (azure-devops--work-item-links
                       work-item related-work-items))
      ;; Recompute Org's buffer-local TODO machinery after inserting the
      ;; file-level directive into an already open buffer.
      (org-set-regexps-and-options)
      (save-buffer)
      (azure-log this-command "Rename file: %s -> %s" (format "%d-Not-yet-updated" id) (format "%d-%s" id filename))
      (rename-visited-file (format "%d-%s" id filename))
      (org-fold-hide-drawer-all))))

(defun azure-devops--work-item-web-url (work-item)
  "Return the Azure DevOps web URL for WORK-ITEM."
  (or (cdr (assoc 'href
                  (cdr (assoc 'html
                              (cdr (assoc '_links work-item))))))
      (format
       "https://dev.azure.com/%s/%s/_workitems/edit/%s"
       (url-hexify-string azure-organization)
       (url-hexify-string azure-project)
       (cdr (assoc 'id work-item)))))

(defun azure-devops--work-item-web-link (work-item)
  "Return an Org link to WORK-ITEM in Azure DevOps."
  (format "[[%s][Open in Azure DevOps]]\n\n"
          (azure-devops--work-item-web-url work-item)))

(defun azure-devops--work-item-links (work-item related-work-items)
  "Return the final Links section for WORK-ITEM and RELATED-WORK-ITEMS."
  (concat "* Links\n\n"
          (azure-devops--work-item-web-link work-item)
          (azure-devops--work-item-relations related-work-items)))

(defun azure-devops-work-item (id)
  "Show the work-item with ID in a buffer of its own.

When called interactively, retrieve the current work-item list immediately
and offer completion candidates showing state, assignee, title, and ID.
Typing in the completion interface narrows that list incrementally.

See URL `https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/work-items/get-work-item'
for more information."
  (interactive (list (azure-devops--read-work-item-id)))
  (azure-log this-command "Show work-item with id: %S" id)
  (funcall 'azure-devops--update-work-item-buffer id))

;; [[https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/work-items/create][Create]]


(defun azure-devops-work-item-create (item-type title)
  "Create a new work-item by specifying ITEM-TYPE and TITLE.

See URL `https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/work-items/create'
for more information."
  (interactive (list (completing-read "Item type: " '("Epic" "Issue" "Task"))
                     (read-from-minibuffer "Item title: ")))
  (let ((url (concat "https://dev.azure.com/{organization}/{project}/_apis/wit/workitems/$" item-type))
        (title (format "%s" title)))
    (azure-post url
                (cl-function
                 (lambda (&key data &allow-other-keys)
                   (azure-devops-work-item (cdr (assoc 'id data)))))
                `((("op" . "add")
                   ("path" . "/fields/System.title")
                   ("from" . nil)
                   ("value" . ,title)))
                '(("api-version" . "7.1-preview.3"))
                '(("Content-Type" . "application/json-patch+json")))))

;; [[https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/work-items/get-work-item][Get Work Item]]


(defun azure-devops--work-item-get (id)
  "Get all relevant information about the work item identified by ID.

See URL `https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/work-items/get-work-item'
for more information."
  (promise-new
   (lambda (resolve _reject)
     (azure-get (format "https://dev.azure.com/{organization}/{project}/_apis/wit/workitems/%d" id)
                (cl-function
                 (lambda (&key data &allow-other-keys)
                   (let ((this-command "azure-devops--work-item-get"))
                    (progn (azure-log this-command "Work item: %S" data)
                           (funcall resolve data)))))
                '(("$expand" . "All")
                  ("api-version" . "7.1-preview.3"))))))

(provide 'azure-devops)
;;; azure-devops.el ends here
