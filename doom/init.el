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
       (eval +overlay)
       lookup

       :os
       (:if (featurep :system 'macos) macos)

       :lang
       emacs-lisp
       markdown
       (org +roam2
            +noter
            +pandoc
            +present)
       sh

       :config
       pkm
       (default +bindings +smartparens))

(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 50))
