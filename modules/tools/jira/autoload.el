;;; tools/jira/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my-jira/get-issue-struct (project type summary description epic labels priority assignee &optional parent-id)
  "Create an issue struct for PROJECT, of TYPE, with SUMMARY and DESCRIPTION."
  (if (or (equal project "")
          (equal type "")
          (equal summary ""))
      (error "Must provide all information!"))

  (let* ((project-components (jiralib-get-components project))
         ;; (core '(id . ,my-jira/component-id))
         (ticket-struct
          `((fields
             (project (key . ,project))
             (parent (key . ,parent-id))
             ;; (components (,core))
             (components ((id . ,my-jira/component-id)))
             (customfield_10005 . ,epic)
             (labels ,labels)
             (issuetype (id . ,(car (rassoc type (if (and (boundp 'parent-id) parent-id)
                                                     (jiralib-get-subtask-types)
                                                   (jiralib-get-issue-types))))))
             (summary . ,(format "%s%s" summary
                                 (if (and (boundp 'parent-id) parent-id)
                                     (format " (subtask of [jira:%s])" parent-id)
                                   "")))
             (description . ,description)
             (assignee (name . ,assignee))
             (priority (id . ,priority))))))

    ticket-struct))

;;;###autoload
(defun my-jira/create-story (project type summary description epic labels priority assignee)
  "Create an issue in PROJECT, of type TYPE, with given SUMMARY and DESCRIPTION."
  (interactive
   (let* ((project my-jira/project-id)
          (type "Story")
          (labels "")
          (summary (read-string "Summary: "))
          (description (read-string "Description: "))
          (epic (read-string "Epic: "))
          (priority (car (rassoc (org-jira-read-priority) (jiralib-get-priorities))))
          (assignee))
     (list project type summary description epic qa-board priority assignee)))

  (if (or (equal project "")
          (equal type "")
          (equal summary ""))
      (error "Must provide all information!"))

  (let* ((parent-id nil)
         (ticket-struct (my-jira/get-issue-struct project type summary description epic labels priority assignee)))

    (message "ticket-struct: %s" ticket-struct)
    (message "json: %s" (json-encode ticket-struct))

    (jiralib-create-issue ticket-struct)
))

;;;###autoload
(defun my-jira/stories-from-buffer ()
  "create jira stories for current buffer"
  (interactive)
  (let* ( (epic (jk-org-kwd "EPIC"))
          (qa-board (jk-org-kwd "QA"))
          (priority (jk-org-kwd "JIRA-PRIORITY"))
          (labels (jk-org-kwd "LABELS"))
          (assignee (jk-org-kwd "ASSIGNEE"))
          (issue-updates nil))

      (org-map-entries
        (lambda ()
          (let* ((summary (nth 4 (org-heading-components)))
                 (tags (nth 5 (org-heading-components)))
                 (description)
                 ;; prefer heading epic property over file level one
                 (epic (or (org-entry-get (point) "EPIC") epic))
                 (assignee (or (org-entry-get (point) "ASSIGNEE") assignee))
                 (priority (or (org-entry-get (point) "JIRA-PRIORITY") priority))
                 (qa-board (or (org-entry-get (point) "QA") qa-board))
                 (created (or (org-entry-get (point) "CREATED") "N"))
                 (struct)
                 (level (car (org-heading-components)))
                 (response nil)
                 (story-id nil))
            (when (eq level 1)
              (if (not (string-equal created "N"))
                  (message "story: %s already created before, skip." summary)

                (message "process: %s" summary)
                ;; translate priority from string to number in jira
                (setq priority (car (rassoc priority (jiralib-get-priorities))))
                (org-gfm-export-as-markdown nil t)
                (setq description (buffer-string*  "*Org GFM Export*"))
                (kill-buffer "*Org GFM Export*")

                (setq response
                      (my-jira/create-story
                       my-jira/project-id "Story" summary
                       description epic labels priority assignee))
                (when response
                   (setq story-id (assoc 'key response))
                   (message "story-id: %s" story-id)
                   (message "summary: %s" (assoc 'summary (assoc 'fields response)))
                   (org-set-property "CREATED" "Y")
                   (save-buffer))

                (message "response for create story: %s" response)
            ))
    )))
))

;;;###autoload
(defun my-jira/stories-from-file (file)
  "create jira stories for `file'"
  (interactive)

  (find-file file)
  (my-jira/stories-from-buffer))

;;;###autoload
(defun my-jira/stories-from-dir (dir)
  "create jira stories for all the org files found under `dir'"
  (interactive)

  (require 'find-lisp)
  (mapc (lambda (f)
          (message "process stories from file: %s" f)
          (my-jira/stories-from-file f))
        (find-lisp-find-files dir "\\.org$")))

;;;###autoload
(defun my-jira/issues-from-jql (jql)
  "Get JQL-LIST list of issues from a custom JQL and PROJ-KEY.

The PROJ-KEY will act as the file name, while the JQL will be any
valid JQL to populate a file to store PROJ-KEY results in.

Please note that this is *not* concurrent or race condition
proof.  If you try to run multiple calls to this function, it
will mangle things badly, as they rely on globals DEFAULT-JQL and
ORG-JIRA-PROJ-KEY-OVERRIDE being set before and after running."
  (message "jql: %s" jql)
  (let* ((parent-id nil)
         (jqls (list jql)))
    (org-jira-get-issues-from-custom-jql jqls)
))

;;;###autoload
(defun my-jira/stories-for-epic (epic)
  (interactive
   (let* ((epic (read-string "Epic: ")))
     (list epic)))

  (let* ((query (format "project = \"%s\"
                    and creator = %s and \"Epic Link\" = %s
                    order by created desc" my-jira/project my-jira/username epic))
          (jql `(:jql ,query :limit 30 :filename ,epic)))

  (my-jira/issues-from-jql jql)))

;;;###autoload
(defun my-jira/stories-for-sprint (sprint)
  (interactive
   (let* ((sprint (read-string "Sprint: ")))
     (list sprint)))

  (let* ((query (format "project = \"%s\"
                    and creator = %s and component in (\"%s\") and Sprint in (\"%s\")
                    order by created desc"
                    my-jira/project my-jira/username my-jira/component sprint))
          (jql `(:jql ,query :limit 30 :filename ,sprint)))

  (my-jira/issues-from-jql jql)))

;;;###autoload
(defun my-jira/stories-for-current-sprint ()
  (interactive)

  (let* ((query (format "project = \"%s\"
                    and creator = %s and component in (\"%s\") and Sprint in openSprints()
                    order by created desc"
                    my-jira/project my-jira/username my-jira/component))
          (jql `(:jql ,query :limit 30 :filename "current-sprint")))

  (my-jira/issues-from-jql jql)))

;;;###autoload
(defun my-jira/stories-for-backlog ()
  (interactive)

  (let* ((query (format "project = \"%s\" and creator = %s and
                        component in (\"%s\") and
                        (sprint is EMPTY or sprint in closedSprints() and
                        sprint not in (openSprints())) and status != Done
                        and reporter=%s order by created desc"
                    my-jira/project my-jira/username my-jira/username))
          (jql `(:jql ,query :limit 30 :filename "backlog")))

  (my-jira/issues-from-jql jql)))

;;;###autoload
(defun my-jira/last-N-created (n)
  (interactive "nHow many stories:")

  (let* ((query (format "project = \"%s\" and creator = %s order by created desc"
                    my-jira/project my-jira/username))
          (jql `(:jql ,query :limit ,n :filename "last-n")))

  (my-jira/issues-from-jql jql)))
