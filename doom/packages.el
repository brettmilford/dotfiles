;; -*- no-byte-compile: t; -*-
;;; .doom.d/packages.el

;; org-mode stuff
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

(package! jiralib2)

;; prog-mode stuff
(package! salt-mode)
(package! jsonnet-mode)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))

