;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;; Dashboard

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

;;;; Input

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
(if window-system
    (when IS-MAC
      (pixel-scroll-precision-mode 1)
      (setq pixel-scroll-precision-use-momentum nil
            pixel-scroll-precision-interpolate-page nil
            pixel-scroll-precision-large-scroll-height 40.0))
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

(setq calc-angle-mode 'rad
      calc-algebraic-mode t
      calc-symbolic-mode t)

(setq vc-follow-symlinks t)
(setq frame-resize-pixelwise t)
(setq confirm-kill-emacs nil)

;;;; Navigation

(map! :n "C-." '+eshell/toggle)

(defun +c/evil-tmux-navigate (direction)
  "Navigate to the window in DIRECTION, falling through to tmux if at edge."
  (let ((cmd (concat "windmove-" direction)))
    (condition-case nil
        (funcall (intern cmd))
      (error
       (+c/evil-tmux-command direction)))))

(defun +c/evil-tmux-command (direction)
  "Send a select-pane command to tmux in DIRECTION."
  (shell-command-to-string
    (concat "tmux select-pane -"
      (upcase (substring direction 0 1)))))

(if (display-graphic-p)
    (map!
     (:after evil
      :enm "C-h"   #'evil-window-left
      :enm "C-j"   #'evil-window-down
      :enm "C-k"   #'evil-window-up
      :enm "C-l"   #'evil-window-right))
  (map!
   (:after evil
     :enm "C-h"   #'(lambda () (interactive) (+c/evil-tmux-navigate "left"))
     :enm "C-j"   #'(lambda () (interactive) (+c/evil-tmux-navigate "down"))
     :enm "C-k"   #'(lambda () (interactive) (+c/evil-tmux-navigate "up"))
     :enm "C-l"   #'(lambda () (interactive) (+c/evil-tmux-navigate "right")))))

(when (and IS-MAC (display-graphic-p))
  (map! "s-n" #'make-frame
        "s-w" #'delete-frame))

(defun +c/toggle-zoom ()
  "Toggle zoom state of the current window.
Uses a per-window parameter so multiple windows can be zoomed independently."
  (interactive)
  (if (window-parameter nil 'zoomed)
      (progn (winner-undo)
             (set-window-parameter nil 'zoomed nil))
    (doom/window-maximize-buffer)
    (set-window-parameter nil 'zoomed t)))

(map! :map evil-window-map
      :desc "Zoom window" "z" #'+c/toggle-zoom)

;;;; Theme

(when (executable-find "defaults")
  (with-temp-buffer
    (call-process "defaults" nil t nil "read" "-g" "AppleInterfaceStyle")
    (if (string-match-p "Dark" (buffer-string))
        (setq doom-theme 'modus-vivendi)
      (setq doom-theme 'modus-operandi))))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(defun +c/toggle-theme ()
  "Toggle between light and dark variants of the current theme."
  (interactive)
  (cond ((eq doom-theme 'modus-vivendi)
         (disable-theme doom-theme)
         (setq doom-theme 'modus-operandi)
         (load-theme doom-theme t))
        ((eq doom-theme 'modus-operandi)
         (disable-theme doom-theme)
         (setq doom-theme 'modus-vivendi)
         (load-theme doom-theme t))
        ((string-suffix-p "-light" (symbol-name doom-theme))
         (disable-theme doom-theme)
         (setq doom-theme (intern (string-remove-suffix "-light" (symbol-name doom-theme))))
         (load-theme doom-theme t))
        ((string-suffix-p "-dark" (symbol-name doom-theme))
         (disable-theme doom-theme)
         (setq doom-theme (intern (concat (string-remove-suffix "-dark" (symbol-name doom-theme)) "-light")))
         (load-theme doom-theme t))
        (t
         (disable-theme doom-theme)
         (setq doom-theme (intern (concat (symbol-name doom-theme) "-light")))
         (load-theme doom-theme t))))

(map! "C-x t" '+c/toggle-theme)
(add-hook 'mac-effective-appearance-change-hook '+c/toggle-theme)

;;;; Fonts & UI

(setq-default line-spacing 0.1)
(setq doom-font (font-spec :family "Iosevka")
      doom-serif-font (font-spec :family "Iosevka Slab")
      doom-variable-pitch-font (font-spec :family "Iosevka Aile"))

(after! doom-modeline
  (setq doom-modeline-unicode-fallback nil
        doom-modeline-percent-position nil
        doom-modeline-icon nil
        doom-modeline-buffer-encoding nil
        doom-modeline-version nil
        doom-modeline-height 15))

(setq display-line-numbers-type nil)

;;;; Auth & Security

(after! auth-source
  (setq auth-sources (nreverse auth-sources)))

(use-package! pinentry
  :init (pinentry-start))

;;;; Outline

(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
(setq outline-minor-mode-cycle t)

;;;; Editor

(defun +c/search-buffer ()
  "Search buffer with consult-line, registering the pattern with evil for n/N."
  (interactive)
    (consult-line)
    (when-let* ((pattern (car consult--line-history))
                (pat (evil-ex-make-search-pattern pattern)))
      (setq evil-ex-search-pattern pat
            evil-ex-search-direction 'forward)
      (evil-push-search-history pattern 'forward)
      (evil-ex-search-activate-highlight pat)))

(map! :nv "/" #'+c/search-buffer)

(after! highlight-indent-guides
  (remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode))

(setq projectile-project-search-path '(("~/src" . 2)))
(setq magit-repository-directories projectile-project-search-path)

(after! spell-fu
  (set-face-attribute 'spell-fu-incorrect-face nil :inherit 'unspecified))

(after! evil-snipe
  (evil-snipe-mode -1))

(after! avy
  (map! :n "s" #'evil-avy-goto-char-2
        :n "S" #'evil-avy-goto-line
        :o "s" #'evil-avy-goto-char-2
        :v "s" #'evil-avy-goto-char-2))

(after! projectile
  (projectile-register-project-type 'nixflake '("flake.nix")
                                  :compile "darwin-rebuild switch --flake .#"
                                  :run "nix develop"))

(after! clipetty
  (setq clipetty-tmux-ssh-tty "tmux show-environment -g SSH_TTY"))

;;;; TRAMP

(setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

(setq tramp-copy-size-limit (* 1024 1024)
      tramp-verbose 2)

(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))

(connection-local-set-profiles
 '(:application tramp :protocol "rsync")
 'remote-direct-async-process)

(setq magit-tramp-pipe-stty-settings 'pty)

(after! tramp
  (with-eval-after-load 'compile
    (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))

;;;; Magit

(after! magit
  (defun +c/magit-auto-revert-not-remote (orig-fun &rest args)
    "Skip auto-revert for remote files to avoid TRAMP overhead."
    (unless (and buffer-file-name (file-remote-p buffer-file-name))
      (apply orig-fun args)))

  (advice-add 'magit-turn-on-auto-revert-mode-if-desired
              :around
              #'+c/magit-auto-revert-not-remote)

  (setq magit-commit-show-diff nil
        magit-branch-direct-configure nil
        magit-refresh-status-buffer nil))

;;;; VTerm

(after! vterm
  (when IS-MAC (setq vterm-shell "/bin/zsh"))
  (setq vterm-max-scrollback 10000)
  (map! :map vterm-mode-map
        :e "C-c w" doom-leader-workspace-map
        :e "C-c C-w" evil-window-map
        :e "C-c c" #'+vterm/here)
  (evil-set-initial-state 'vterm-mode 'emacs))
