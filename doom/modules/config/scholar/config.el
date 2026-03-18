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
