;;; org-notification.el --- Org Event Notifications -*- lexical-binding: t; -*-
;;; example usage: /usr/bin/env emacs --fg-daemon -Q -l ~/.config/doom/lisp/org-notification.el

;;; Code:
;(message (format "Loading user lisp %s" (file-name-base buffer-file-name)))
;(setq debug-on-error t)

(require 'org)
(setq org-directory (expand-file-name "~/org"))
(setq org-agenda-files (list org-directory))
;; NOTE: won't pickup entries w/o a time specificed
(unless +pkm-encrypt-enabled
  (run-at-time 60 1200 'org-agenda-to-appt))
;(org-agenda-to-appt t nil :deadline :deadline* :scheduled :scheduled* :timestamp)

(require 'appt)
(setq appt-time-msg-list nil
  appt-display-interval '5
  appt-message-warning-time '15
  appt-display-mode-line nil
  appt-display-format 'window
  appt-disp-window-function (function appt-display-native))
(appt-activate 1)

(defun send-notification (headline min-to-app)
  (let ((notifier-path (executable-find "terminal-notifier")))
      (start-process
          "Notification"
          nil ;; nil to turn off appt alert buffer
          notifier-path
          "-message" min-to-app
          "-title" headline
          "-sender" "org.gnu.Emacs"
          "-activate" "org.gnu.Emacs")))

(defun linux-send-notification (headline min-to-app)
  (let ((notifier-path (executable-find "notify-send")))
      (start-process
          "Notification"
          nil ;; nil to turn off appt alert buffer
          notifier-path
          "-a" "Emacs"
          "-i" "/usr/share/icons/hicolor/32x32/apps/emacs.png"
          "-u" "normal"
          headline
          min-to-app)))

(defun appt-display-native (min-to-app new-time msg)
  (ignore new-time)
  (if (string-equal system-type "darwin")
  (send-notification
    (format "%s" msg)
    (format "Due in %s minutes" min-to-app))
  (linux-send-notification
    (format "%s" msg)
    (format "Due in %s minutes" min-to-app))))

(provide 'org-notification)
;;; org-notification.el ends here
