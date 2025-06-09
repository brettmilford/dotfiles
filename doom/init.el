;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       ;;(corfu +orderless)
       company
       vertico

       :ui
       doom
       doom-dashboard
       hl-todo ;; TODO/HACK/FIXME/REVIEW/NOTE/DEPRECATED/BUG/XXX
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
       biblio
       (eval +overlay)
       lookup
       magit
       direnv
       lsp
       tree-sitter

       :os
       (:if (featurep :system 'macos) macos)
       (tty +osc)

       :lang
       emacs-lisp
       sh
       markdown
       (org +roam2
            +noter
            +pandoc
            +present)
       latex
       (rest +jq)
       yaml
       json
       data
       nix
       (cc +lsp
           +tree-sitter)
       (go +lsp
           +tree-sitter)
       (rust +lsp
             +tree-sitter)
       (zig +lsp
            +tree-sitter)

       :term
       eshell
       ;vterm

       :config
       private
       assistant
       pkm
       (default +bindings +smartparens))

(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 50))
