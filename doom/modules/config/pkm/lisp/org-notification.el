;;; org-notification.el --- Org Event Notifications -*- lexical-binding: t; -*-

;;; Commentary:

;; Native desktop notifications for org-mode appointments.
;; Can be used standalone: emacs --fg-daemon -Q -l org-notification.el

;;; Code:

(require 'org)
(setq org-directory (expand-file-name "~/org"))
(setq org-agenda-files (list org-directory))

(unless +pkm-encrypt-enabled
  (run-at-time 60 1200 'org-agenda-to-appt))

(require 'appt)
(setq appt-time-msg-list nil
  appt-display-interval '5
  appt-message-warning-time '15
  appt-display-mode-line nil
  appt-display-format 'window
  appt-disp-window-function #'org-notification--display-native)
(appt-activate 1)

(defun org-notification--send-darwin (headline min-to-app)
  "Send a macOS notification with HEADLINE due in MIN-TO-APP minutes."
  (let ((notifier-path (executable-find "terminal-notifier")))
      (start-process
          "Notification"
          nil
          notifier-path
          "-message" min-to-app
          "-title" headline
          "-sender" "org.gnu.Emacs"
          "-activate" "org.gnu.Emacs")))

(defun org-notification--send-linux (headline min-to-app)
  "Send a Linux notification with HEADLINE due in MIN-TO-APP minutes."
  (let ((notifier-path (executable-find "notify-send")))
      (start-process
          "Notification"
          nil
          notifier-path
          "-a" "Emacs"
          "-i" "/usr/share/icons/hicolor/32x32/apps/emacs.png"
          "-u" "normal"
          headline
          min-to-app)))

(defun org-notification--display-native (min-to-app new-time msg)
  "Display appointment MSG due in MIN-TO-APP minutes as a native notification.
NEW-TIME is ignored."
  (ignore new-time)
  (let ((headline (format "%s" msg))
        (body (format "Due in %s minutes" min-to-app)))
    (if (eq system-type 'darwin)
        (org-notification--send-darwin headline body)
      (org-notification--send-linux headline body))))

(provide 'org-notification)
;;; org-notification.el ends here
