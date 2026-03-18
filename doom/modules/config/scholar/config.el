;;; config/scholar/config.el -*- lexical-binding: t; -*-
;;; --- Scholar: reading & research platform

;;;; Variables

(defvar scholar-library-dir (expand-file-name "library" org-directory)
  "Root directory for scholar library nodes.")

(defvar scholar-elfeed-enabled nil
  "When non-nil, enable elfeed integration.")

(defvar scholar-lit-project-dir nil
  "Path to the default lit project directory for PDF/academic import.")

(defvar scholar-discussion-sources '(hn hypothesis)
  "List of discussion sources to query. Options: hn, hypothesis, reddit, mastodon, lobsters.")

(defvar scholar-mastodon-instance nil
  "Mastodon instance URL for discussion aggregation.")

(defvar scholar-mastodon-token nil
  "Bearer token for Mastodon API.")

(defvar scholar-reddit-client-id nil
  "Reddit OAuth client ID for discussion aggregation.")

;;;; Org-protocol

(after! org
  (add-to-list 'org-protocol-protocol-alist
               '("scholar-capture"
                 :protocol "scholar-capture"
                 :function +scholar--org-protocol-capture)))

;;;###autoload (autoload '+scholar--org-protocol-capture "config/scholar/autoload" nil t)

;;;; Dashboard mode (derived from magit-section-mode)

(require 'magit-section)

(defvar +scholar-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "r") #'+scholar/dashboard-refresh)
    (define-key map (kbd "RET") #'+scholar/dashboard-open-item)
    (define-key map (kbd "a") #'+scholar/dashboard-add-url)
    (define-key map (kbd "s") #'+scholar/search-library)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for the scholar dashboard buffer.")

(define-derived-mode +scholar-dashboard-mode magit-section-mode "Scholar"
  "Major mode for the scholar library dashboard."
  :group 'scholar)

;;;; Keybindings

(map!
 :leader
 :prefix ("n" . "notes")
 (:prefix ("s" . "scholar")
  :desc "Dashboard" "s" #'+scholar/dashboard
  :desc "Add URL" "a" #'+scholar/dashboard-add-url
  :desc "Search library" "/" #'+scholar/search-library
  :desc "Set read state" "r" #'+scholar/set-read-state))

;;;; Reading mode auto-activation

(defun +scholar--maybe-enable-reading-mode ()
  "Enable +scholar-reading-mode if file is a library node."
  (when (and buffer-file-name
             (string-prefix-p (expand-file-name scholar-library-dir)
                              (expand-file-name buffer-file-name)))
    (+scholar-reading-mode 1)))

(add-hook 'org-mode-hook #'+scholar--maybe-enable-reading-mode)
