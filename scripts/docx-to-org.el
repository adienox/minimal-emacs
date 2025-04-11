;; yoinked from https://www.kostaharlan.net/posts/google-docs-to-org-mode/

(defun nox/export-google-doc-to-org-buffer ()
  "Prompt for a Google Docs URL, export it to DOCX using `gdrive`, convert to Org using `pandoc`, and open the content in a new buffer."
  (interactive)
  (let* ((url (read-string "Enter Google Docs URL: "))
         (doc-id (if (string-match "document/d/\\([^/]+\\)" url)
                     (match-string 1 url)
                   (error "Invalid URL format")))
         (temp-dir (file-name-as-directory temporary-file-directory))
         (docx-file (concat temp-dir doc-id ".docx"))
         (org-file (concat temp-dir doc-id ".org"))
         (gdrive-info-command (concat "gdrive files info " doc-id))
         (gdrive-export-command (concat "gdrive files export --overwrite " doc-id " " docx-file))
         (pandoc-command (concat "pandoc --wrap=none -f docx " docx-file " -t org -o " org-file))
         (buffer-name nil)
         (title nil)
         (created nil)
         (updated nil)
         (gdrive-output-buffer "*GDrive Output*")
         (url nil))

    ;; Ensure the temp directory exists
    (unless (file-directory-p temp-dir)
      (make-directory temp-dir t))

    ;; Get document info from gdrive
    (with-temp-buffer
      (shell-command gdrive-info-command t gdrive-output-buffer)
      (goto-char (point-min))
      (when (re-search-forward "^Name: \\(.*\\)" nil t)
        (setq title (match-string 1))
        (message "Title: %s" title))
      (when (re-search-forward "^Modified: \\(.*\\)" nil t)
        (setq updated (match-string 1))
        (message "Updated: %s" updated))
      (when (re-search-forward "^Created: \\(.*\\)" nil t)
        (setq created (match-string 1))
        (message "Created: %s" created))
      (when (re-search-forward "^ViewUrl: \\(.*\\)" nil t)
        (setq url (match-string 1))
        (message "URL: %s" url)))

    ;; Check if title and updated are nil
    (unless (and title updated)
      (error "Failed to retrieve document information from gdrive"))

    ;; Shell out to gdrive to export the document
    (with-temp-buffer
      (shell-command gdrive-export-command t gdrive-output-buffer)
      (message "Export command output: %s" (buffer-string))
      (goto-char (point-min))
      (when (re-search-forward "Exporting document '\\([^']+\\)'" nil t)
        (setq buffer-name (concat (match-string 1) ".org"))
        (message "Buffer Name: %s" buffer-name)))

    ;; Check if buffer-name is nil
    (unless buffer-name
      (error "Failed to export document from gdrive"))

    ;; Shell out to pandoc to convert the DOCX to Org
    (shell-command pandoc-command)

    (message "here")

    ;; Check if Org file was created
    (unless (file-exists-p org-file)
      (error "Org file was not created by pandoc command"))

    ;; Create properties drawer content
    (setq properties-content
          (concat ":PROPERTIES:\n"
                  ":TITLE: " title "\n"
                  ":CREATED: " created "\n"
                  ":UPDATED: " updated "\n"
                  ":URL: " url "\n"
                  ":END:\n\n"))

    ;; Create a new buffer and insert the contents of the exported Org file
    (with-current-buffer (get-buffer-create buffer-name)
      (insert properties-content)
      (insert-file-contents org-file)

      ;; Remove empty lines between bullet points
      (goto-char (point-min))
      (while (re-search-forward "^\n\\(-\\)" nil t)
        (replace-match "\\1" nil nil))

      ;; Remove empty lines between numbered list items
      (goto-char (point-min))
      (while (re-search-forward "^\n\\([0-9]+\\.\\)" nil t)
        (replace-match "\\1" nil nil))

      ;; Open the buffer
      (switch-to-buffer (current-buffer))

      ;; Set the buffer to Org mode
      (org-mode))))

(provide 'docx-to-org)
