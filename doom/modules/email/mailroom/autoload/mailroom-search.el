;;; email/mailroom/autoload/mailroom-search.el -*- lexical-binding: t; -*-

;;;###autoload
(defun mailroom-search (&optional global-p)
  "Search email using mu query syntax with completion.
If GLOBAL-P is non-nil, search across all accounts.
Otherwise, restrict search to the current folder."
  (interactive "P")
  (let* ((current-folder (when (and (not global-p)
                                    (eq major-mode 'mu4e-headers-mode))
                           (mu4e-last-query)))
         (prompt (if global-p "Search (all): " "Search: "))
         (query (read-string prompt)))
    (when (not (string-empty-p query))
      (mailroom-layout--focus-pane 'list)
      (if (and current-folder (not global-p))
          ;; Scope to current maildir
          (let* ((maildir (when (string-match "maildir:\\([^ ]+\\)" current-folder)
                            (match-string 1 current-folder))))
            (if maildir
                (mu4e-headers-search (format "(%s) AND maildir:%s" query maildir))
              (mu4e-headers-search query)))
        (mu4e-headers-search query)))))

;;;###autoload
(defun mailroom-search-global ()
  "Search across all email accounts."
  (interactive)
  (mailroom-search t))

;;;###autoload
(defun mailroom-search-save ()
  "Save the current search query as a saved search."
  (interactive)
  (let ((query (when (eq major-mode 'mu4e-headers-mode)
                 (mu4e-last-query))))
    (unless query
      (setq query (read-string "Query to save: ")))
    (let ((name (read-string (format "Name for \"%s\": " query))))
      (push (list :name name :query query) mailroom-saved-searches)
      (mailroom-sidebar--render)
      (message "Saved search: %s" name))))

;;;###autoload
(defun mailroom-search-jump-to-saved ()
  "Jump to the saved searches section in the sidebar."
  (interactive)
  (mailroom-layout--focus-pane 'sidebar)
  (with-current-buffer mailroom-sidebar-buffer-name
    (goto-char (point-max))
    (search-backward "Saved Searches" nil t)))
