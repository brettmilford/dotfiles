;;; email/mailroom/autoload/mailroom-layout.el -*- lexical-binding: t; -*-

(defvar mailroom-layout--active nil
  "Non-nil when the mailroom tri-panel layout is active.")

(defvar mailroom-welcome-buffer-name "*mailroom-welcome*"
  "Name of the welcome buffer shown in the view pane.")

;;;###autoload
(defun mailroom-layout--create ()
  "Create the tri-panel layout: sidebar | headers | view."
  (delete-other-windows)
  ;; Create sidebar
  (let* ((sidebar-win (selected-window))
         (rest-win (split-window sidebar-win (or (bound-and-true-p mailroom-sidebar-width) 30) 'right))
         (list-width (round (* (window-total-width rest-win)
                               (or (bound-and-true-p mailroom-list-width-fraction) 0.35))))
         (list-win rest-win)
         (view-win (split-window list-win list-width 'right)))
    ;; Set up sidebar
    (mailroom-sidebar--render)
    (set-window-buffer sidebar-win (get-buffer mailroom-sidebar-buffer-name))
    (set-window-dedicated-p sidebar-win t)
    ;; Set up view pane with welcome screen
    (mailroom-layout--show-welcome view-win)
    (set-window-dedicated-p view-win t)
    ;; Focus the message list pane
    (select-window list-win)
    ;; Start mu4e headers in list pane (show INBOX of first account)
    (let* ((accounts (mailroom-sidebar--get-accounts))
           (first-root (plist-get (car accounts) :root)))
      (when first-root
        (mu4e-headers-search (format "maildir:%s/INBOX" first-root))))
    (setq mailroom-layout--active t)))

;;;###autoload
(defun mailroom-layout--show-welcome (window)
  "Display a welcome screen in WINDOW."
  (let ((buf (get-buffer-create mailroom-welcome-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "\n\n")
        (insert (propertize "  Mailroom" 'face '(:height 1.5 :weight bold)) "\n\n")
        (insert "  Select a folder or message to get started.\n\n")
        (insert (propertize "  Keybindings:\n" 'face 'bold))
        (insert "  h/l    — switch panes\n")
        (insert "  j/k    — navigate\n")
        (insert "  /      — search current folder\n")
        (insert "  g /    — search all accounts\n")
        (insert "  g r    — refresh mail\n")
        (insert "  c      — compose new message\n")
        (insert "  q      — quit mailroom\n"))
      (special-mode))
    (set-window-buffer window buf)))

;;;###autoload
(defun mailroom-layout--get-pane (type)
  "Get the window for pane TYPE: 'sidebar, 'list, or 'view."
  (cl-case type
    (sidebar (get-buffer-window mailroom-sidebar-buffer-name))
    (list    (get-buffer-window (or (bound-and-true-p mu4e-headers-buffer-name)
                                       "*mu4e-headers*")))
    (view    (or (get-buffer-window (or (bound-and-true-p mu4e-view-buffer-name)
                                           "*mu4e-article*"))
                 (get-buffer-window mailroom-welcome-buffer-name)
                 ;; Compose buffers live in the view pane too
                 (cl-loop for win in (window-list)
                          when (with-current-buffer (window-buffer win)
                                 (derived-mode-p 'mu4e-compose-mode
                                                 'org-msg-edit-mode))
                          return win)))))

;;;###autoload
(defun mailroom-layout--restore-if-broken ()
  "Restore the tri-panel layout if a pane was accidentally killed."
  (when mailroom-layout--active
    (unless (and (mailroom-layout--get-pane 'sidebar)
                 (mailroom-layout--get-pane 'list))
      (mailroom-layout--create))))

;;;###autoload
(defun mailroom-layout--ensure-view-pane ()
  "Return the view pane window, recreating it if it was killed.
Splits the list pane to create a new view pane on the right."
  (or (mailroom-layout--get-pane 'view)
      (when-let ((list-win (mailroom-layout--get-pane 'list)))
        (let ((view-win (split-window list-win nil 'right)))
          (mailroom-layout--show-welcome view-win)
          view-win))))

;;;###autoload
(defun mailroom-layout--focus-pane (type)
  "Move focus to pane TYPE: 'sidebar, 'list, or 'view."
  (when-let ((win (mailroom-layout--get-pane type)))
    (select-window win)))
