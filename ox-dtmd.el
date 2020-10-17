;;; ox-dtmd.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 emacsbliss
;;
;; Author: emacsbliss <http://github.com/emacsbliss>
;; Maintainer: emacsbliss
;; Created: October 16, 2020
;; Modified: October 16, 2020
;; Version: 0.0.1
;; Keywords:
;; Package-Requires: ((emacs 27.0.50) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(require 'ox-md)
(require 'ox-gfm)

(defgroup org-export-dtmd nil
  "Options specific to Markdown export back-end."
  :tag "Org to Devonthink Flavored Markdown"
  :group 'org-export
  :version "24.4"
  :package-version '(Org . "8.0"))

(org-export-define-derived-backend 'dtmd 'gfm
  :filters-alist '((:filter-parse-tree . org-md-separate-elements))
  :menu-entry
  '(?d "Export to Devonthink Flavored Markdown"
       ((?D "To temporary buffer"
            (lambda (a s v b) (org-dtmd-export-as-markdown a s v)))
        (?d "To file" (lambda (a s v b) (org-dtmd-export-to-markdown a s v)))
        (?o "To file and open"
            (lambda (a s v b)
              (if a (org-dtmd-export-to-markdown t s v)
                (org-open-file (org-dtmd-export-to-markdown nil s v)))))))
  :translate-alist '((src-block . org-dtmd-src-block)
                     (link . org-dtmd-link)
                     ))

;;;###autoload
(defun org-dtmd-convert-region-to-md ()
  "Assume the current region has org-mode syntax, and convert it
to Github Flavored Markdown.  This can be used in any buffer.
For example, you can write an itemized list in org-mode syntax in
a Markdown buffer and use this command to convert it."
  (interactive)
  (org-export-replace-region-by 'gfm))

;;;###autoload
(defun org-dtmd-export-to-markdown (&optional async subtreep visible-only)
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'dtmd outfile async subtreep visible-only)))

;;;###autoload
(defun org-dtmd-export-as-markdown (&optional async subtreep visible-only)
  (interactive)
  (org-export-to-buffer 'dtmd "*Org DTMD Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

(defun org-dtmd-src-block (src-block contents info)
  "Transcode SRC-BLOCK element into Github Flavored Markdown
format. CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let* ((lang (org-element-property :language src-block))
         (code (org-export-format-code-default src-block info))
         (prefix (concat "```language-" lang "\n"))
         (suffix "```"))
    (concat prefix code suffix)))

(defvar org-dt/db "SlipBox")
(defvar org-dt/group "/test-1")
(defvar org-dt/current-title nil)

(defun org-dtmd-get-devon-id-title (file-name)
  (with-current-buffer (find-file-noselect file-name)
    (list (jk-org-kwd "DEVON-ID")
          (jk-org-kwd "TITLE"))
))

(defun org-dtmd-link (link desc info)
  "Transcode LINK object into Markdown format.
DESC is the description part of the link, or the empty string.
INFO is a plist holding contextual information.  See
`org-export-data'."
  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         (uuid-title nil))

    (message "org-dt/current-title: %s" org-dt/current-title)
    (message "desc: %s" desc)
    (if (string-equal desc org-dt/current-title)
        (progn
          (message "link to self")
          ;; return nothing for self link
          (format "**%s**" desc))

          (if (string-equal  type "file")
            (if (org-export-inline-image-p link)
                ;; TODO: later we may need to create the image
                ;; in devonthink and get uuid back
                (org-md-link link desc info)

                (progn
                  (message "raw-path: %s" raw-path)
                  (setq uuid-title (org-dtmd-get-devon-id-title raw-path))
                  (if (car uuid-title)
                    (format "[%s](x-devonthink-item://%s)" desc (car uuid-title))
                    ;; Need to create the file and get the id
                    (progn
                      (with-current-buffer (find-file-noselect raw-path)
                      (message "create [%s] with placeholder" desc)
                      (format "[%s](x-devonthink-item://%s)" desc
                        (org-dt/create-md-and-update-org desc "placeholder"))))))
            )

          ;; not file type link
          ;; fall back to org-md for handle rest of link type
          (org-md-link link desc info))
    )
))

(defun org-dt/create-md-in-dt (name)
  (do-applescript
   (concat
    "tell application id \"DNtp\"\n"
    "set theName to \"" name "\"\n"
    "set theMD to (get the clipboard)\n"
    "set theDB to (database named \"" org-dt/db "\")\n"
    "set theFolder to (get record at \"" org-dt/group "\" in theDB)\n"
    "set newRecord to (create record with {name:theName, type:markdown, content:theMD} in theFolder)\n"
    "set docId to uuid of newRecord\n"
    "end tell\n"
    "return docId as string"
)))

(defun org-dt/update-md-in-dt (uuid)
  (do-applescript
   (concat
    "tell application id \"DNtp\"\n"
    "set theRecord to get record with uuid \"" uuid "\"\n"
    "set theMD to (get the clipboard)\n"
    "set plain text of theRecord to theMD\n"
    "end tell"
)))

(defun org-dt/export-md ()
  (let* ((export-buffer "*temp-export*"))

    (org-export-to-buffer 'dtmd export-buffer)
    (with-current-buffer export-buffer
      (goto-char (point-min))
      (insert
"<link rel=\"stylesheet\" href=\"x-devonthink-item://B58D8A66-2572-44BB-BB55-932EA0A28D2E\"/>
<link rel=\"stylesheet\" href=\"x-devonthink-item://84207DDE-C706-4CE1-899C-7B938D6B443A\"/>
<script src=\"x-devonthink-item://20EA3C53-C5D7-4BC3-A067-D5203B2C48FD\"></script>
<section class=\"line-numbers\">\n\n")
;; {{TOC}}\n\n")

      (buffer-substring-no-properties (point-min) (point-max))
)))

(defun org-dt/export ()
  "export current buffer to DT3"
  (interactive)

  ;; NOTE: detect if file already have DEVON-ID,
  ;; if yes, then we should update instead of creating
  ;; new file in DT3
  (let* ((devon-id (jk-org-kwd "DEVON-ID"))
         (title (jk-org-kwd "TITLE"))
         (org-buffer (current-buffer))
         (md-content "placeholder"))

    (setq org-dt/current-title title)
    (setq md-content (org-dt/export-md))
    (set-buffer org-buffer)
    (message "devon-id here: %s" devon-id)
    (message "org-dt/current-title: %s" org-dt/current-title)

    (if devon-id
        (progn
        (message "update [%s]" title)
        (kill-new md-content)
        (org-dt/update-md-in-dt devon-id))

      (progn
        (message "create [%s] with real content" title)
        (org-dt/create-md-and-update-org title md-content)
      ))
))

(defun org-dt/create-md-and-update-org (title content)
  (kill-new content)
  (setq devon-id (org-dt/create-md-in-dt title))
  (message "devon-id: %s" devon-id)
  (goto-char (point-min))
  (insert "#+DEVON-ID: " devon-id "\n")
  (save-buffer)
  devon-id)

(provide 'ox-dtmd)
;;; ox-dtmd.el ends here
