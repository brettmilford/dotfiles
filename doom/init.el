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
       (format +lsp)
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
       llm
       lsp
       tree-sitter
       (docker +lsp +tree-sitter)

       :os
       (:if (featurep :system 'macos) macos)
       (tty +osc)

       :lang
       emacs-lisp
       (sh +lsp)
       (markdown +tree-sitter)
       (org +roam2
            +noter
            +pandoc
            +present)
       (latex +lsp)
       (rest +jq)
       (yaml +lsp
             +tree-sitter)
       (json +lsp
             +tree-sitter)
       data
       (nix +lsp
            +tree-sitter)
       (cc +lsp
           +tree-sitter)
       (go +lsp
           +tree-sitter)
       (rust +lsp
             +tree-sitter)
       (javascript +lsp
                   +tree-sitter)

       :term
       eshell
       vterm

       :config
       private
       assistant
       pkm
       (default +bindings +smartparens))

(add-to-list 'default-frame-alist '(width . 100))
(add-to-list 'default-frame-alist '(height . 50))
