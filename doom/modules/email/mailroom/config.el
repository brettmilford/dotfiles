;;; email/mailroom/config.el -*- lexical-binding: t; -*-

(defvar mailroom-saved-searches nil
  "List of saved searches displayed in the sidebar.
Each entry is a plist with :name and :query keys.
Example: '((:name \"Unread (7d)\" :query \"flag:unread date:7d..now\"))")

(defvar mailroom-auto-refresh-interval nil
  "Interval in seconds for automatic refresh. nil to disable.")

(defvar mailroom-accounts-file
  (expand-file-name "mailroom-accounts.el" doom-user-dir)
  "Path to the private file containing email account definitions.
This file should contain `set-email-account!' calls and should be
gitignored to keep email addresses out of public repos.")

(defvar mailroom-sidebar-width 30
  "Width of the sidebar window in columns.")

(defvar mailroom-list-width-fraction 0.35
  "Fraction of remaining width for the message list pane.")

;; Load private account config
(after! mu4e
  (when (file-exists-p mailroom-accounts-file)
    (load mailroom-accounts-file nil 'nomessage)))

;; Route mu4e buffers to correct mailroom panes
(after! mu4e
  ;; Override mu4e-display-buffer to route view buffers to the view pane.
  ;; mu4e passes its own action list to display-buffer which overrides
  ;; display-buffer-alist, so we advise mu4e-display-buffer directly.
  (define-advice mu4e-display-buffer (:around (fn buffer-or-name &optional select) mailroom-route)
    "Route mu4e buffers to mailroom panes when layout is active."
    (if (not mailroom-layout--active)
        (funcall fn buffer-or-name select)
      (let* ((buffer (get-buffer buffer-or-name))
             (buf-type (with-current-buffer buffer
                         (mu4e--get-current-buffer-type))))
        (cond
         ;; View buffers go to the view pane
         ((eq buf-type 'view)
          (let ((win (mailroom-layout--ensure-view-pane)))
            (when win
              (set-window-dedicated-p win nil)
              (set-window-buffer win buffer)
              (set-window-dedicated-p win t)
              (when select (select-window win)))
            (or win (funcall fn buffer-or-name select))))
         ;; Headers stay in the list pane
         ((eq buf-type 'headers)
          (let ((win (mailroom-layout--get-pane 'list)))
            (if win
                (progn
                  (set-window-buffer win buffer)
                  (when select (select-window win))
                  win)
              (funcall fn buffer-or-name select))))
         ;; Everything else uses default behavior
         (t (funcall fn buffer-or-name select))))))
  ;; Compose buffers go to the view pane
  (add-hook! 'mu4e-compose-mode-hook
    (defun mailroom--route-compose-to-view-h ()
      (when mailroom-layout--active
        (when-let ((view-win (mailroom-layout--get-pane 'view)))
          (set-window-buffer view-win (current-buffer))
          (select-window view-win)))))
  ;; After sending, restore the view pane
  (add-hook! 'message-sent-hook
    (defun mailroom--after-send-h ()
      (when mailroom-layout--active
        (run-at-time 0.1 nil
                     (lambda ()
                       (when-let ((view-win (mailroom-layout--get-pane 'view)))
                         (mailroom-layout--show-welcome view-win)
                         (mailroom-layout--focus-pane 'list))))))))

;; Mailroom keybindings
;; Sidebar has its own mode, no conflict with evil-collection
(after! mu4e
  (map! :map mailroom-sidebar-mode-map
        :n "j"   #'next-line
        :n "k"   #'previous-line
        :n "RET" #'mailroom-sidebar-open-at-point
        :n "TAB" #'mailroom-sidebar-toggle-account
        :n "o"   #'mailroom-sidebar-next-account
        :n "h"   #'mailroom-focus-left
        :n "l"   #'mailroom-focus-right
        :n "q"   #'mailroom-quit
        :n "g r" #'mailroom-refresh
        :n "c"   #'mailroom-change-folder)

  ;; Headers and view bindings must override evil-collection
  (after! evil-collection-mu4e
    (map! :map mu4e-headers-mode-map
          :n "h"   #'mailroom-focus-left
          :n "l"   #'mailroom-focus-right
          :n "RET" #'mu4e-headers-view-message
          :n "q"   #'mailroom-quit
          :n "g r" #'mailroom-refresh
          :n "j"   (cmd! (mu4e-headers-next) (mailroom--preview-follows-cursor-h))
          :n "k"   (cmd! (mu4e-headers-prev) (mailroom--preview-follows-cursor-h))
          ;; Compose
          :n "r"   #'mu4e-compose-reply
          :n "R"   (cmd! (mu4e-compose-reply t)) ; reply-all
          :n "f"   #'mu4e-compose-forward
          :n "C"   #'mu4e-compose-new
          ;; Marks (mutt-style)
          :n "d"   #'mu4e-headers-mark-for-trash
          :n "s"   #'mu4e-headers-mark-for-flag
          :n "!"   #'mu4e-headers-mark-for-unread
          :n "t"   #'mailroom-tag-message
          :n "u"   #'mailroom-undelete
          :n "$"   #'mailroom-sync-marks
          :n "x"   #'mu4e-mark-execute-all
          ;; Folder / view
          :n "c"   #'mailroom-change-folder
          :n "T"   #'mailroom-toggle-threads
          :n "L"   #'mailroom-limit-view
          :n "A"   #'mailroom-unlimit-view)

    ;; View pane keybindings
    (map! :map mu4e-view-mode-map
          :n "h"   #'mailroom-focus-left
          :n "l"   #'mailroom-focus-right
          :n "q"   (cmd! (when-let ((win (mailroom-layout--get-pane 'view)))
                           (mailroom-layout--show-welcome win))
                         (mailroom-layout--focus-pane 'list))
          :n "j"   #'evil-next-visual-line
          :n "k"   #'evil-previous-visual-line
          :n "J"   #'mu4e-view-headers-next
          :n "K"   #'mu4e-view-headers-prev
          :n "r"   #'mu4e-compose-reply
          :n "R"   (cmd! (mu4e-compose-reply t))
          :n "f"   #'mu4e-compose-forward
          :n "d"   #'mu4e-headers-mark-for-trash
          :n "o"   #'+mu4e-view-open-attachment)

    ;; Search keybindings (add to sidebar, headers, and view maps)
    (dolist (map (list 'mu4e-headers-mode-map
                       'mu4e-view-mode-map))
      (map! :map (symbol-value map)
            :n "/"   #'mailroom-search
            :n "g /" #'mailroom-search-global
            :n "g s" #'mailroom-search-jump-to-saved
            :n "g S" #'mailroom-search-save)))

  ;; Sidebar search bindings (outside evil-collection-mu4e)
  (map! :map mailroom-sidebar-mode-map
        :n "/"   #'mailroom-search
        :n "g /" #'mailroom-search-global
        :n "g s" #'mailroom-search-jump-to-saved
        :n "g S" #'mailroom-search-save))

;; Auto-refresh timer
(after! mu4e
  (when mailroom-auto-refresh-interval
    (run-with-timer mailroom-auto-refresh-interval
                    mailroom-auto-refresh-interval
                    #'mailroom-refresh)))
