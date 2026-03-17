;; -*- no-byte-compile: t; -*-
;;; config/collab/packages.el

(package! websocket)

(package! collab
  :recipe (:host nil
           :repo "git@calliope:emacs-collab.git"
           :files ("collab/*.el")))

(package! annotate
  :recipe (:host nil
           :repo "git@calliope:emacs-collab.git"
           :files ("annotate/*.el" "annotate/annotate-merge.sh")))

(package! collab-annotate
  :recipe (:host nil
           :repo "git@calliope:emacs-collab.git"
           :files ("collab-annotate/*.el")))
