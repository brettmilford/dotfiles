;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(add-to-list 'term-file-aliases '("alacritty" . "xterm"))
(setq mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed t
      mouse-wheel-follow-mouse t
      scroll-step 1
      scroll-margin 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

;; Enable mouse support
(unless window-system
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(setq vc-follow-symlinks t)
(setq frame-resize-pixelwise t)
(setq confirm-kill-emacs nil)

;; NOTE: toggle with doom/toggle-line-numbers (SPC t l)
(setq display-line-numbers-type nil)

(map! :n "/" 'swiper)
(map! :n "C-." '+eshell/toggle)

(if (display-graphic-p)
    (map!
     (:after evil
      :enm "C-h"   #'evil-window-left ;; NOTE: need to remap help
      :enm "C-j"   #'evil-window-down
      :enm "C-k"   #'evil-window-up
      :enm "C-l"   #'evil-window-right))
  (progn
    (defun evil-tmux-navigate (direction)
      (let
          ((cmd (concat "windmove-" direction)))
        (condition-case nil
            (funcall (read cmd))
          (error
           (evil-tmux-command direction)))))

    (defun evil-tmux-command (direction)
      (shell-command-to-string
        (concat "tmux select-pane -"
          (evil-tmux-direction direction))))

    (defun evil-tmux-direction (direction)
      (upcase
        (substring direction 0 1)))
    (map!
     (:after evil
       :en "C-h"   #'(lambda ()
                       (interactive)
                       (evil-tmux-navigate "left"))
       :en "C-j"   #'(lambda ()
                       (interactive)
                       (evil-tmux-navigate "down"))
       :en "C-k"   #'(lambda ()
                       (interactive)
                       (evil-tmux-navigate "up"))
       :en "C-l"   #'(lambda ()
                       (interactive)
                       (evil-tmux-navigate "right"))))))

(map! "C-x ?" 'help-command) ;; NOTE: 'SPC h .' does the same
(map! "C-x /" '(lambda() (interactive) (find-file "~/nix-systems/config/doom/config.el")))

(defun merge-init () (interactive)(ediff-merge-files "~/.config/doom/init.el" "~/.emacs.d/init.example.el"))

(if (and IS-MAC (display-graphic-p))
         (map! "s-n" #'make-frame
               "s-w" #'delete-frame))

;; use light theme when running in apple_terminal
;; TODO: revist, doesn't quite work
;(unless (and (not(display-graphic-p))
;             (and (string= (getenv "TERM_PROGRAM") "Apple_Terminal")
;                  (not (string= (getenv "MACOS_DARKMODE") "true"))))
;    (load-theme (intern (concat (symbol-name doom-theme) "-light")) t))

(load-theme 'doom-horizon t)
;; Slightly increase contrast
;(set-face-attribute 'default nil :foreground "#CFCFCF")

(after! doom-themes
  (setq
   doom-themes-enable-bold t
   doom-themes-enable-italic t))

(defun toggle-theme ()
  "Toggle light/dark of the current theme."
  (interactive)
  (if (string-suffix-p "-light" (symbol-name doom-theme))
      (load-theme (intern (string-remove-suffix "-light" (symbol-name doom-theme))) t)
    (if (string-suffix-p "-dark" (symbol-name doom-theme))
      (load-theme (intern (concat (string-remove-suffix "-dark" (symbol-name doom-theme)) "-light")) t)
      (load-theme (intern (concat (symbol-name doom-theme) "-light")) t)))
  (doom/reload-theme))

(map! "C-x t" 'toggle-theme)

;; TODO: for GUI emacs, might need to change this based on display size
(setq doom-font (font-spec :family "Ubuntu Mono" :size 16))
;(setq doom-serif-font (font-spec :family "Ubuntu" :size 4))

(after! doom-modeline
  (setq doom-modeline-unicode-fallback nil)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-version nil)
  (setq doom-modeline-height 15))

;; TODO: see https://github.com/hlissner/doom-emacs/issues/3398
(add-hook! 'tty-setup-hook #'xclip-mode)

;; C-x * (c e k q) (C-x * ? ? for all)
(setq calc-angle-mode 'rad
      calc-algebraic-mode t
      calc-symbolic-mode t)

;; NOTE: auto-fill is generally preferred over visual-line-mode as its faster
(add-hook! 'text-mode-hook #'+word-wrap-mode)
;; TODO: this doesn't work in markdown?
;; needs to happen before markdown mode?
;(remove-hook 'markdown-mode-hook #'auto-fill-mode)
(remove-hook 'text-mode-hook #'auto-fill-mode)

;(defun diff-init()
;  "Diff current init file with init.example.el"
;  )

;; Packages
(setq projectile-discover-projects-in-directory '("~/prjs"))
(after! projectile
  (add-to-list 'projectile-known-projects "~/Dropbox/Apps/Editorial/"))
  ;(setq projectile-tags-command "ctags -R \"%s\" %s \"%s\""))


(map! :leader
      "p ]" '+ivy/project-search)

(setq magit-repository-directories '(("~/prjs" . 3) ("~/.emacs.d" . 0) ("~/.password-store" . 1)))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org.gpg\\'"     . org-mode))

(defun open-sf-at-point ()
  ;(defconst URL "http://googlelinks.canonical.com/case?c=")
  ;(defconst URL "https://canonical--c.um6.visual.force.com/apex/caseredirect?c=")
  (let* ((text (thing-at-point 'word))
         (URL "https://canonical.my.salesforce.com/apex/caseredirect?c="))
    (cond ((string-match "^SF[0-9].......$" text)
           (let* ((match (match-string 0 text))
                  (sfurl (concat URL (substring match 2 (length match)))))
             (message (format "Opening: %s" sfurl))
             (org-open-link-from-string sfurl))
           t)
          ((string-match "@[0-9].....$" text)
           (let* ((match (match-string 0 text))
                  (sfurl (concat URL "00" (substring match 1 (length match)))))
             (message (format "Opening: %s" sfurl))
             (org-open-link-from-string sfurl))
           t)
          (t
           nil))))

(defun open-lp-at-point ()
  (let* ((text (thing-at-point 'word)) ;; 'filename is hack to prevent breaking on '#'
         (URL "http://pad.lv/"))
    (cond ((string-match "^[l,L][p,P][#]?[0-9]......$" text)
           (let* ((match (match-string 0 text))
                  (lpurl (concat URL (substring match 2 (length match)))))
             (message (format "Opening: %s" lpurl))
             (org-open-link-from-string lpurl))
           t)
          (t
           nil))))

(setq org-directory (file-truename"~/notes/org"))
(after! org
  ;(map! "C-c o" '(lambda() (interactive) (counsel-find-file org-directory))) ;; NOTE: 'SPC n f' does the same
  ;
  (map! "C-x S" 'org-save-all-org-buffers) ;; NOTE: 'SPC h .' does the same
  (map!
   :map org-mode-map
   :localleader
        "TAB" #'org-insert-structure-template)
  (add-hook! 'org-mode-hook #'+word-wrap-mode)
  ;(add-hook! 'org-mode-hook #'incremental-reading-mode)
  (remove-hook 'org-mode-hook #'auto-fill-mode)
  (add-to-list 'org-open-at-point-functions 'open-sf-at-point)
  (add-to-list 'org-open-at-point-functions 'open-lp-at-point)
  ;; moving notes/org to ~/org to compensate for all the config vars
  ;(setq org-directory (expand-file-name "~/notes/org"))
  (setq org-agenda-window-setup 'reorganize-frame)
  (setq org-agenda-files (list org-directory))
  (setq org-default-notes-file (expand-file-name "01-inbox.org" org-directory))
  (setq org-columns-default-format "%25ITEM %3PRIORITY %TODO %SCHEDULED %DEADLINE %TAGS")
  (setq org-fontify-done-headline t)
  (setq org-agenda-view-columns-initially nil)
  (setq org-agenda-custom-commands
        '(("d" "Todo and Due" ((org-ql-block '(or (and (todo) (scheduled :to 0))
                                              (and (todo) (priority "A")))
                                         ((org-ql-block-header "Todo and Due")))
                                         (agenda)))))
  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-cache nil)
  (setq org-refile-target-verify-function (lambda ()
                                            "Filters out Archive nodes"
                                            (if (string= (nth 4 (org-heading-components)) "Archive")
                                                    (unless (ignore-errors (org-forward-element))
                                                      (goto-char (point-max))) t)))
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-startup-folded 't)
  (setq org-tags-column -77)
  ;(setq org-archive-location "::* Archive")
  (setq org-cycle-open-archived-trees 't)
  (setq org-adapt-indentation nil)
  (setq org-startup-indented t)
  ;(setq org-hide-leading-stars t)
  (setq org-pretty-entities 't)
  (setq org-log-done 'time)
  ;(setq org-log-repeat nil)
  (setq org-enforce-todo-dependencies 't)
  ;(setq org-latex-compiler "xelatex")
  (setq org-latex-bib-compiler "biber")
  (setq org-latex-pdf-process
        '("%latex -interaction nonstopmode -output-directory %o %f"
          "%bib %b"
          "%latex -interaction nonstopmode -output-directory %o %f"
          "%latex -interaction nonstopmode -output-directory %o %f"))
  ;; TODO: Tryout org-mode speed commands
  ;(setq org-use-speed-commands 't)
  ;(setq org-export-date-timestamp-format "[%Y-%m-%d %a]")
  (setq org-export-date-timestamp-format "%B %e, %Y")
  (setq org-todo-keywords
  '((sequence "TODO(!)" "INPROGRESS(!)" "WAIT(w@/!)" "|" "DONE" "CANCELLED")
    ;("WOEng(e/!) WOUps(u/!) WOCus(c/!) WOOps(o/!) WOSup(s/!) | RESOLVED DEESCLATED REASSIGNED")
    ))
  ;(setq org-log-into-drawer 't) ;;TODO: Check if this breaks clock logging
  ;(add-to-list org-todo-keyword-faces `("WOCUS" . +org-todo-onhold))
  ;(add-to-list org-todo-keyword-faces '("WOSUP" . +org-todo-onhold))
  ;(add-to-list org-todo-keyword-faces '("WOOPS" . +org-todo-onhold))
  ;(add-to-list org-todo-keyword-faces '("WOUPS" . +org-todo-onhold))
  (setq org-log-done 't)
  (setq org-todo-keywords-for-agenda org-todo-keywords)
  (setq org-table-duration-custom-format 'minutes)
  (setq org-clock-persist 't)
  (org-clock-persistence-insinuate)
  (setq org-clock-continuously nil) ;doesn't work the way I thought
  (setq org-clock-persist-query-resume nil)
  (setq org-clock-out-when-done t)
  (setq org-clock-report-include-clocking-task t)
  (setq org-html-self-link-headlines t)
  (setq org-use-tag-inheritance nil)
  (setq org-crypt-key "brettmilford@gmail.com")
  (setq org-capture-templates
      '(("t" "Task" entry (file org-default-notes-file)
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+olp+datetree "~/org/90-journal.org")
         "* %U %? :crypt:\n  %i\n")
        ("b" "BA Entry" entry (file+olp+datetree "~/org/90-journal.org")
         "* %u :BA:crypt:%i\n# List every activity and rate them on a scale of 1 to 10\nMorning\n-%?\n\nAfternoon\n-\n\nEvening\n-")
        ("r" "BA Reflection" entry (file+olp+datetree "~/org/90-journal.org")
         "* %u :BA:crypt:\n  %i\n%?")
        ("h" "Health" entry (file+olp+datetree "~/org/90-journal.org")
         "* %U %? :health:\n  %i\n" :unnarrowed t))))

(after! deft
  (setq deft-directory "~/notes")
  (setq deft-recursive t)
  (setq deft-default-extension "org")
  (setq deft-use-filter-string-for-filename t))

(setq org-roam-directory (file-truename "~/notes"))
(after! org-roam
  ;; makes id links work, if org-mode hasn't cached them
  (org-id-update-id-locations (org-roam-list-files) 't)
  (setq org-roam-mode-sections
        '((org-roam-backlinks-section :unique -t)
           org-roam-reflinks-section
          ;org-roam-unlinked-references-section))
          ))
  ;;(setq +org-roam-auto-backlinks-buffer nil)
  (setq org-roam-db-location "~/.emacs.d/.local/cache/org-roam.db")
  (setq org-roam-buffer-no-delete-other-windows 't)
  (setq org-roam-completion-system 'ivy)
  ;; if needed & lots of gc taking place
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  ;; set use directories as tags
  (setq org-roam-tag-sources '(prop all-directories))
  (setq org-roam-dailies-capture-templates
        '(("d" "Journal" entry "* %? \t:crypt:\n%U\n"
           :if-new (file+head "%<%Y-%m-%d>.org"
                                  "#+title: %<%A the %e of %B %Y>\n#+filetags: %<:%Y:%B:>\n"
                                  ))
          ("h" "Health" entry "* %? :health:\n%U\n"
           :if-new (file+head "%<%Y-%m-%d>.org"
                                  "#+title: %<%A the %e of %B %Y>\n#+filetags: %<:%Y:%B:>\n"
                                  ))
          )
        )
  (setq org-roam-capture-templates
        '(("d" "default" plain
          "%?"
          :target (file+head "${cxt=.}/%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+FILETAGS:\n\n- topics ::")
          :unnarrowed t))))

(after! org-roam-graph
  (if IS-MAC
   (setq org-roam-graph-viewer "open")
   (setq org-roam-graph-viewer "xdg-open")))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam
    :config
    (map!
     :leader
      (:prefix ("n" . "notes")
        (:prefix ("r" . "roam")
          :desc "Org Roam UI" "u" (lambda () (interactive)
                                    (if (member '(org-roam-ui-mode " org-roam-ui") minor-mode-alist)
                                                 (org-roam-ui-open)
                                                 (org-roam-ui-mode))))))

    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; TODO: revisit: only being used for keyword formatting
;(use-package! org-roam-bibtex
;  :after org-roam
;  ;:hook (org-roam-mode . org-roam-bibtex-mode)
;  :config
;  ;(require 'org-ref)
;  (setq orb-preformat-keywords
;   '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
;  (setq orb-process-file-field 't)
;  (setq orb-insert-link-description 'citation-org-ref-2)
;;  (add-to-list 'org-roam-capture-templates
;;        '("r" "bibliography reference" plain "%?
;;"
;;           :target (file+head "annotations/${citekey}.org"
;;                              "#+TITLE: ${title}
;;#+FILETAGS: ${keywords}
;; topics ::
;;
;;* ${title}
;;:PROPERTIES:
;;:Custom_ID: ${citekey}
;;:URL: ${url}
;;:AUTHOR: ${author-or-editor}
;;:NOTER_DOCUMENT: ${file}
;;:NOTER_PAGE:
;;:END:")
;;           :unnarrowed t))
;)

(setq reftex-default-bibliography '("~/notes/references.bib"))

(after! bibtex-completion
  ;(advice-add 'bibtex-completion-candidates
  ;            :filter-return 'reverse)
  (setq bibtex-completion-notes-path "~/notes/annotations"
        bibtex-completion-library-path "~/notes/fulltext"
        bibtex-completion-bibliography '("~/notes/references.bib"
                                         "~/notes/canonical/optimising_resource_allocation_in_hyper_converged_infrastructure/references.bib"
                                         "~/notes/canonical/references.bib"))
  (setq bibtex-completion-notes-template-multiple-files
        (concat
         ":PROPERTIES:\n"
         ":ROAM_ALIASES: ${=key=}\n"
         ":ROAM_REFS: @${=key=}\n"
         ":END:\n"
         "#+TITLE: ${title}\n"
         "#+FILETAGS: ${keywords}\n\n"
         "- topics :: \n"
         "* ${title}\n"
         ":PROPERTIES:\n"
         ":Custom_ID: ${=key=}\n"
         ":URL: ${url}\n"
         ":AUTHOR: ${author-abbrev}\n"
         ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
         ":NOTER_PAGE:"
         ":JOURNAL: ${journaltitle}\n"
         ":DATE: ${date}\n"
         ":YEAR: ${year}\n"
         ":DOI: ${doi}\n"
         ":END:\n\n"))
  (add-hook 'bibtex-completion-notes-mode-hook #'org-id-get-create))

(use-package! org-ref
  :after bibtex-completion)

(after! bibtex
  (setq bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator ""
        bibtex-autokey-year-title-separator ""
        bibtex-autokey-titleword-separator ""
        bibtex-autokey-titlewords 1
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length nil))

(after! org-noter
  (setq org-noter-notes-search-path "~/notes/annotations/"))

(use-package! ox-reveal
  :after org-mode)

;(use-package! nov
;  :init
;  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(after! anki-editor
       (setq anki-editor-create-decks t))

(after! org-mode-incremental-reading
  (add-hook! 'incremental-reading-mode-hook #'anki-editor-mode)
  (add-to-list
   'org-protocol-protocol-alist
   '("org-open-file" :protocol "open-file" :function org-protocol-open-file))
  (defun org-protocol-open-file (fname)
  "Process an org-protocol://open-file?url= style URL with FNAME.
  Change a filename by mapping URLs to local filenames as set
  in `org-protocol-project-alist'.
  The location for a browser's bookmark should look like this:
  javascript:location.href = \\='org-protocol://open-file?url=\\=' + \\
        encodeURIComponent(location.href)"
  ;; As we enter this function for a match on our protocol, the return value
  ;; defaults to nil.
    (let ((f (org-protocol-sanitize-uri
              (plist-get (org-protocol-parse-parameters fname nil '(:file))
                         :file))))
      f)))

(after! spell-fu
  (set-face-attribute 'spell-fu-incorrect-face nil :inherit 'unspecified))

(after! ispell
  ;(setq ispell-personal-dictionary nil)
  )

(after! ivy
 (setq ivy-count-format "(%d/%d) ")
 (setq ivy-use-virtual-buffers t))

;; TODO: should hook to dired mode
;(add-hook! 'dired-mode-hook #'(lambda ()
;                               (turn-off-evil-snipe-mode)))
(after! evil-snipe
  (evil-snipe-mode -1))

(after! company
  (setq company-idle-delay nil))

(after! projectile
  (projectile-register-project-type 'nixosflake '("flake.nix")
                                  :project-file "flake.nix"
                                  :compile "darwin-rebuild switch --flake ~/nix-systems#"
                                  :test "npm test"
                                  :run "nix develop"
                                  :test-suffix ".spec"))
(setq-hook! 'nix-mode-hook
  counsel-compile-history '("darwin-rebuild switch --flake ~/nix-systems#"))

;; Load workspaces
;(add-hook 'doom-init-ui-hook (lambda () (+workspace-load "org")))

;; Load scripts into daemon
(load! "lisp/org-notification")
