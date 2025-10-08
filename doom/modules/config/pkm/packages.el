;; -*- no-byte-compile: t; -*-
;;; config/pkm/packages.el

(package! org-ql)
(package! ox-reveal)
(package! org-modern)
(package! org-roam-ui)
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(package! org-ref)
(package! nov
  :recipe (:host nil :repo "https://depp.brause.cc/nov.el.git"))
(package! anki-editor)
(package! org-mode-incremental-reading
  :recipe (:host github :repo "vascoferreira25/org-mode-incremental-reading"))
(package! mermaid-mode) ;; markdown diagrams
(package! sqlite3)
(package! jiralib2)
