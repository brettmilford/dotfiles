;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq frame-title-format '("%b – Emacs"))
(setq +doom-dashboard-name "*Dash*")
(setq +doom-dashboard-ascii-banner-fn
(lambda ()
  (let* ((banner '("
                    ##@@@##  ##@@##                        ##@@@#
            #     ##@@@@@@@@@@@@@@@@#                    #@@@@@@@#
           #@####@@@#  ##@@@#####@@@@@#                ##@@## #@@@
          #@@@@@@@#     ####      ##@@@@##  ##@       ####     @@#
           ##@@##     ###           ##@@@@@@@@##@###@@#       #@@
                     #@#        ###    ######  @@@@@@#        @@#
                    #@#     ## #@@@#           #####         #@#
                  #@@#   ##@## #@@@@#                        @@
                  @@@###@@#      #@@#                       #@#
                 @@@@@@##        #@@#                       @#
                 #@@##           #@@               #       #@#
                     ###@@@@@@   ##               ##       @#
                ###@@@@@###@@#     ###          #@#       #@#
              #@@@@##   ####      #@#          #@#        #@
            #@@@@#              #@@#          #@#        #@#
           #@@@@    ###        #@@       #@@#@@          #@
          #@@@@@@#@@##       #@@#       #@@@@#           @#
           ##@@@@@##        #@@#       @@####   #        #   ##    ###
           #@@##      #@@##@@@# ##@@# @#    ######      ##@@##########
         #@@#      ##@@@#@@@@@@@@@@@###  ##@###@@#    #@@@##  #@@@######
       #@@@     ####@@@@@@##@@###@@# ###@@#  #@@@   #@@@@           #@@#
      #@@@@####@###@@@@@# #@@# #@@@####@@@###@@@@@###@@@@@###########@##
      @@@@@@@@#  #@@@@@#  @@   #@@### #@###  ######  ######  ########
      #@@@##     #####
"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner))))


;; BUG: emacs-mac wide toolbar
(add-hook 'doom-after-init-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0)))

(add-to-list 'term-file-aliases '("alacritty" . "xterm"))
(setq mouse-wheel-scroll-amount '(1)
      mouse-wheel-progressive-speed t
      mouse-wheel-follow-mouse t
      scroll-step 1
      scroll-margin 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
(unless window-system
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; C-x * (c e k q) (C-x * ? ? for all)
(setq calc-angle-mode 'rad
      calc-algebraic-mode t
      calc-symbolic-mode t)

;; NOTE: auto-fill is generally faster than visual-line-mode.
;(add-hook! 'text-mode-hook #'+word-wrap-mode)
;; TODO: this doesn't work in markdown?
;; needs to happen before markdown mode?
;(remove-hook 'markdown-mode-hook #'auto-fill-mode)
;(remove-hook 'text-mode-hook #'auto-fill-mode)
(setq vc-follow-symlinks t)
(setq frame-resize-pixelwise t)
(setq confirm-kill-emacs nil)

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
       :enm "C-h"   #'(lambda ()
                       (interactive)
                       (evil-tmux-navigate "left"))
       :enm "C-j"   #'(lambda ()
                       (interactive)
                       (evil-tmux-navigate "down"))
       :enm "C-k"   #'(lambda ()
                       (interactive)
                       (evil-tmux-navigate "up"))
       :enm "C-l"   #'(lambda ()
                       (interactive)
                       (evil-tmux-navigate "right"))))))

;(map! "C-x ?" 'help-command) ;; NOTE: 'SPC h .' does the same

(if (and IS-MAC (display-graphic-p))
         (map! "s-n" #'make-frame
               "s-w" #'delete-frame))

(defvar +c/zoomed nil)

(defun +c/toggle-zoom ()
  (interactive)
  (cond (+c/zoomed
         (winner-undo)
         (setq +c/zoomed nil))
        ((not +c/zoomed)
         (doom/window-maximize-buffer)
         (setq +c/zoomed 't))))

(map! :map evil-window-map
      :desc "Zoom window" "z" #'+c/toggle-zoom)

;; UI & Theme

;; use light theme when running in apple_terminal
;; TODO: revist, doesn't quite work
;(unless (and (not(display-graphic-p))
;             (and (string= (getenv "TERM_PROGRAM") "Apple_Terminal")
;                  (not (string= (getenv "MACOS_DARKMODE") "true"))))
;    (load-theme (intern (concat (symbol-name doom-theme) "-light")) t))

;; (set-face-attribute 'default nil :foreground "#CFCFCF") ;; Slightly increase contrast
;; (set-face-background 'default "undefined" (selected-frame)) ;; Sets to non-exsistent colour which falls back to terminal bg

;; Theme
;; Set theme on startup
(when (executable-find "defaults")
(with-temp-buffer
  (call-process "defaults" nil t nil "read" "-g" "AppleInterfaceStyle")
  (if (string-match-p "Dark" (buffer-string))
      (setq doom-theme 'modus-vivendi)
      (setq doom-theme 'modus-operandi))))

(after! doom-themes
  (setq
   doom-themes-enable-bold t
   doom-themes-enable-italic t))

(defun toggle-theme ()
  "Toggle light/dark of the current theme."
  (interactive)
  ;; Handle modus themes
  (cond ((eq doom-theme 'modus-vivendi)
         (disable-theme doom-theme)
         (setq doom-theme 'modus-operandi)
         (load-theme doom-theme t))
        ((eq doom-theme 'modus-operandi)
         (disable-theme doom-theme)
         (setq doom-theme 'modus-vivendi)
         (load-theme doom-theme t))
        ;; Handle other themes with -light/-dark suffixes
        ((string-suffix-p "-light" (symbol-name doom-theme))
         (disable-theme doom-theme)
         (setq doom-theme (intern (string-remove-suffix "-light" (symbol-name doom-theme))))
         (load-theme doom-theme t))
        ((string-suffix-p "-dark" (symbol-name doom-theme))
         (disable-theme doom-theme)
         (setq doom-theme (intern (concat (string-remove-suffix "-dark" (symbol-name doom-theme)) "-light")))
         (load-theme doom-theme t))
        ;; Default case: add -light suffix
        (t
         (disable-theme doom-theme)
         (setq doom-theme (intern (concat (symbol-name doom-theme) "-light")))
         (load-theme doom-theme t))))

(map! "C-x t" 'toggle-theme)
;; NOTE: Requires emacs-mac
(add-hook 'mac-effective-appearance-change-hook 'toggle-theme)

(setq-default line-spacing 0.1)
(setq doom-font (font-spec :family "Iosevka")
      doom-serif-font (font-spec :family "Iosevka Slab")
      doom-variable-pitch-font (font-spec :family "Iosevka Etoile"))

(after! vterm
  (evil-set-initial-state 'vterm-mode 'emacs))

(after! doom-modeline
  (setq doom-modeline-unicode-fallback nil)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-version nil)
  (setq doom-modeline-height 15))
;; NOTE: toggle with doom/toggle-line-numbers (SPC t l)
(setq display-line-numbers-type nil)

(after! auth-source
  (setq auth-sources (nreverse auth-sources)))

(use-package! pinentry
  :init (pinentry-start))

(after! highlight-indent-guides
  (remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode))

(defvar devenv 't) ;; docker, vm (ssh), direnv/nix/localhost (t), devcontainer ?
(cond ((eq devenv 'docker) (setq projectile-project-search-path '(("/docker:de:/src" . 2))))
      (t (setq projectile-project-search-path '(("~/src" . 2)))))
(setq magit-repository-directories projectile-project-search-path)

;; NOTE: SPC s p does this
;(map! :leader "p ]" '+ivy/project-search)
(after! evil
  (map! :nv "/" #'+default/search-buffer))

(after! spell-fu
  (set-face-attribute 'spell-fu-incorrect-face nil :inherit 'unspecified))

;; TODO: should hook to dired mode
;(add-hook! 'dired-mode-hook #'(lambda ()
;                               (turn-off-evil-snipe-mode)))
(after! evil-snipe
  (evil-snipe-mode -1))

(after! projectile
  (projectile-register-project-type 'nixflake '("flake.nix")
                                  :compile "darwin-rebuild switch --flake .#"
                                  :run "nix develop"))

(after! clipetty
  (setq clipetty-tmux-ssh-tty "tmux show-environment -g SSH_TTY"))

;; TRAMP performance improvements
(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

(setq tramp-copy-size-limit (* 1024 1024) ;; 1MB
      tramp-verbose 2)

(after! tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))


(after! magit
  (defun $magit-auto-revert-not-remote (orig-fun &rest args)
    (unless (and buffer-file-name (file-remote-p buffer-file-name))
      (apply orig-fun args)))

  ;; Don't auto-revert remote files
  (advice-add 'magit-turn-on-auto-revert-mode-if-desired
              :around
              #'$magit-auto-revert-not-remote)

  ;; don't show the diff by default in the commit buffer. Use `C-c C-d' to display it
  (setq magit-commit-show-diff nil)
  ;; don't show git variables in magit branch
  (setq magit-branch-direct-configure nil)
  ;; don't automatically refresh the status buffer after running a git command
  (setq magit-refresh-status-buffer nil))

;; Fix vterm shell on macOS to use system zsh instead of Nix bash
(when IS-MAC
  (after! vterm
    (setq vterm-shell "/bin/zsh")))

(evil-set-initial-state 'vterm-mode 'emacs)

;; https://github.com/doomemacs/doomemacs/issues/8541
(let ((lfile (concat doom-local-dir "straight/repos/transient/lisp/transient.el")))
  (if (file-exists-p lfile)
      (load lfile)))
