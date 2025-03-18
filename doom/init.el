;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (corfu +orderless)
       vertico

       :ui
       doom
       doom-dashboard
       hl-todo
       modeline
       ophints
       (popup +defaults)
       (vc-gutter +pretty)
       vi-tilde-fringe
       workspaces
       zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       snippets
       ;;word-wrap

       :emacs
       dired
       electric
       undo
       vc

       :checkers
       syntax
       spell

       :tools
       (:if (modulep! :config pkm biblio))
       (eval +overlay)
       lookup
       magit
       direnv

       :os
       (:if (featurep :system 'macos) macos)
       (tty +osc)

       :lang
       nix
       emacs-lisp
       markdown
       (:if (modulep! :config pkm)
        (org +roam2
             +noter
             +pandoc
             +present)
        latex)
       sh
       (rest +jq)
       yaml
       json
       data

       :term
       eshell
       ;vterm

       :config
       pkm
       private
       (default +bindings +smartparens))

(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 50))
