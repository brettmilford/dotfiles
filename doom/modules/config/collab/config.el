;;; config/collab/config.el -*- lexical-binding: t; -*-

;;;; Collaborative Editing (collab-mode)

(use-package! collab
  :commands (collab-start-server collab-connect collab-disconnect
             collab-follow-user collab-unfollow collab-list-users
             collab-undo collab-redo)
  :init
  (setq collab-username user-login-name
        collab-colour "#4a9eff")
  (map! :leader
        (:prefix ("c" . "collab")
         :desc "Start server"   "s" #'collab-start-server
         :desc "Connect"        "c" #'collab-connect
         :desc "Disconnect"     "d" #'collab-disconnect
         :desc "Follow user"    "f" #'collab-follow-user
         :desc "Unfollow"       "F" #'collab-unfollow
         :desc "User list"      "u" #'collab-list-users
         :desc "Undo (collab)"  "z" #'collab-undo
         :desc "Redo (collab)"  "Z" #'collab-redo)))

;;;; Annotations (annotate-mode)

(use-package! annotate
  :commands (annotate-mode annotate-create annotate-list
             annotate-next annotate-prev)
  :hook ((latex-mode  . annotate-mode)
         (LaTeX-mode  . annotate-mode)
         (org-mode    . annotate-mode)
         (markdown-mode . annotate-mode))
  :init
  (map! :leader
        (:prefix ("A" . "annotate")
         :desc "Create annotation" "a" #'annotate-create
         :desc "List annotations"  "l" #'annotate-list
         :desc "Next annotation"   "n" #'annotate-next
         :desc "Previous"          "p" #'annotate-prev)))

;;;; Bridge (auto-activates when both collab and annotate are loaded)

(use-package! collab-annotate
  :after (collab annotate))

(message "config/collab/config.el was evaluated")
