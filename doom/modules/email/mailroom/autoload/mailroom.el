;;; email/mailroom/autoload/mailroom.el -*- lexical-binding: t; -*-

(defvar mailroom--standalone-p nil
  "Non-nil when mailroom was launched in standalone mode.")

;;;###autoload
(defun =mailroom ()
  "Launch mailroom email client.
If :ui workspaces is enabled, opens in a dedicated workspace.
Otherwise, takes over the current frame."
  (interactive)
  (require 'mu4e)
  ;; Ensure mu4e is running
  (unless (mu4e-running-p)
    (mu4e t)) ; start in background
  (if (and (modulep! :ui workspaces)
           (bound-and-true-p persp-mode))
      (+workspace-switch "+mailroom+" t)
    (delete-other-windows))
  (mailroom-layout--create))

;;;###autoload
(defun mailroom-refresh ()
  "Refresh mail: run mbsync, reindex mu, refresh sidebar."
  (interactive)
  (message "Refreshing mail...")
  (let ((proc (start-process "mailroom-refresh" "*mailroom-refresh*"
                             "bash" "-c"
                             mu4e-get-mail-command)))
    (set-process-sentinel
     proc
     (lambda (_proc event)
       (when (string-match-p "finished" event)
         (mu4e-update-index)
         (mailroom-sidebar--render)
         (message "Mail refreshed."))))))

;;;###autoload
(defun mailroom-focus-left ()
  "Move focus to the pane to the left."
  (interactive)
  (let* ((current (selected-window))
         (sidebar (mailroom-layout--get-pane 'sidebar))
         (list-win (mailroom-layout--get-pane 'list))
         (view (mailroom-layout--get-pane 'view)))
    (cond
     ((eq current view) (when list-win (select-window list-win)))
     ((eq current list-win) (when sidebar (select-window sidebar))))))

;;;###autoload
(defun mailroom-focus-right ()
  "Move focus to the pane to the right."
  (interactive)
  (let* ((current (selected-window))
         (sidebar (mailroom-layout--get-pane 'sidebar))
         (list-win (mailroom-layout--get-pane 'list))
         (view (mailroom-layout--get-pane 'view)))
    (cond
     ((eq current sidebar) (when list-win (select-window list-win)))
     ((eq current list-win) (when view (select-window view))))))

;;;###autoload
(defun mailroom-quit ()
  "Quit mailroom. In standalone mode, prompts for confirmation."
  (interactive)
  (when (or (not mailroom--standalone-p)
            (y-or-n-p "Quit Mailroom? "))
    (setq mailroom-layout--active nil)
    ;; Kill mailroom buffers
    (dolist (buf-name (list mailroom-sidebar-buffer-name
                           mailroom-welcome-buffer-name))
      (when-let ((buf (get-buffer buf-name)))
        (kill-buffer buf)))
    (if mailroom--standalone-p
        (kill-emacs)
      ;; Restore previous window config
      (if (and (modulep! :ui workspaces)
               (+workspace-exists-p "+mailroom+"))
          (+workspace/kill "+mailroom+")
        (delete-other-windows)
        (switch-to-buffer (doom-fallback-buffer))))))

;;;###autoload
(defun mailroom--preview-follows-cursor-h ()
  "When moving in the headers pane, preview the message at point in the view pane."
  (when (and mailroom-layout--active
             (eq major-mode 'mu4e-headers-mode)
             (mailroom-layout--get-pane 'view))
    (mu4e-headers-view-message)))

;;; Mutt-like behaviors

;;;###autoload
(defun mailroom-change-folder ()
  "Switch to a different folder with completion (like mutt's `c').
Prompts with all known maildirs."
  (interactive)
  (let ((maildir (completing-read
                  "Change folder: "
                  (mu4e-get-maildirs) nil t)))
    (when maildir
      (mailroom-layout--focus-pane 'list)
      (mu4e-headers-search (format "maildir:%s" maildir)))))

;;;###autoload
(defun mailroom-toggle-threads ()
  "Toggle threaded view in the headers (like mutt's thread collapsing)."
  (interactive)
  (setq mu4e-headers-show-threads (not mu4e-headers-show-threads))
  (mu4e-headers-rerun-search)
  (message "Threads %s" (if mu4e-headers-show-threads "on" "off")))

;;;###autoload
(defun mailroom-limit-view ()
  "Narrow the current headers view by adding a search term (like mutt's `l').
Appends to the current query with AND."
  (interactive)
  (let* ((current (mu4e-last-query))
         (term (read-string (format "Limit (%s) AND: " current))))
    (when (not (string-empty-p term))
      (mu4e-headers-search (format "(%s) AND (%s)" current term)))))

;;;###autoload
(defun mailroom-unlimit-view ()
  "Remove the limit and show all messages in the current folder."
  (interactive)
  (let ((query (mu4e-last-query)))
    ;; Try to extract the original maildir query
    (if (string-match "^(\\(maildir:[^ )]+\\)) AND" query)
        (mu4e-headers-search (match-string 1 query))
      ;; Otherwise just rerun the current search
      (mu4e-headers-rerun-search))))

;;;###autoload
(defun mailroom-tag-message ()
  "Toggle mark on current message and move to next (like mutt's `t').
Uses mu4e's 'something' mark to visually tag messages."
  (interactive)
  (mu4e-headers-mark-for-something)
  (mu4e-headers-next))

;;;###autoload
(defun mailroom-undelete ()
  "Unmark the message at point (like mutt's `u')."
  (interactive)
  (mu4e-headers-mark-for-unmark))

;;;###autoload
(defun mailroom-sync-marks ()
  "Execute all pending marks (like mutt's `$')."
  (interactive)
  (mu4e-mark-execute-all t))
