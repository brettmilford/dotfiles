;;; .doom.d/config.el -*- lexical-binding: t; -*-

(setq +doom-dashboard-ascii-banner-fn
(lambda ()
  (let* ((banner '("
                       :::                                     :
                     :=+*+=- :-=++=-:                       -=+++-:
            ::     :=**+****+*******+-:                   :+******=
           :=*-::-+*+-:::=***=---=+***+-:      ::       ::+*=::-**+
           -******+-:    :-=:      :=****=-:::=+::    :=---:   :+*-
           :-=++=:     :==:          :=*******+::+=--=+-       -*+:
                      -+=        -=:   :-====-::+****+:        +*:
                    :=*-    :--:=***-          :-==-:         :*-
                   :+*-  :-=+=::=+***:                        ++:
                  :+*+:-=++-:    :=**-                       :*:
                 :=****+=:        :**:                       +=
                 :=++=:    ::::   :*=              ::       :*:
                     :-==+++**+:  :=:::           :=-       +=
                 :-=+**++=-=*+-:    :=:          :+-       :*:
              :-+**+=-:  :--:      -+-          -+-        =+
             -+**+-:     ::      :++:      :: :=+:        :*-
           :=***+   ::--:       -*=:      -++=++:         =+:
           :*****+==+=-:      :+*-      :=****=:         :*-
            :=****+-:    :: :-*+:      :++--=:  :::      :-"))
         (longest-line (apply #'max (mapcar #'length banner))))
    (put-text-property
     (point)
     (dolist (line banner (point))
       (insert (+doom-dashboard--center
                +doom-dashboard--width
                (concat line (make-string (max 0 (- longest-line (length line))) 32)))
               "\n"))
     'face 'doom-dashboard-banner))))

(add-hook 'doom-after-init-hook (lambda () (tool-bar-mode 1) (tool-bar-mode 0)))
(defvar config-directory "~/nix-systems/config")
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
(add-hook! 'text-mode-hook #'+word-wrap-mode)
;; TODO: this doesn't work in markdown?
;; needs to happen before markdown mode?
;(remove-hook 'markdown-mode-hook #'auto-fill-mode)
(remove-hook 'text-mode-hook #'auto-fill-mode)
(setq vc-follow-symlinks t)
(setq frame-resize-pixelwise t)
(setq confirm-kill-emacs nil)

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

(map! "C-x ?" 'help-command) ;; NOTE: 'SPC h .' does the same

(if (and IS-MAC (display-graphic-p))
         (map! "s-n" #'make-frame
               "s-w" #'delete-frame))

(defun merge-init() (interactive)
       (ediff-merge-files (expand-file-name "doom/init.el" config-directory) "~/.emacs.d/init.example.el"))

;; UI & Theme

;; use light theme when running in apple_terminal
;; TODO: revist, doesn't quite work
;(unless (and (not(display-graphic-p))
;             (and (string= (getenv "TERM_PROGRAM") "Apple_Terminal")
;                  (not (string= (getenv "MACOS_DARKMODE") "true"))))
;    (load-theme (intern (concat (symbol-name doom-theme) "-light")) t))

(load-theme 'doom-tokyo-night t)
;; (set-face-attribute 'default nil :foreground "#CFCFCF") ;; Slightly increase contrast
;; (set-face-background 'default "undefined" (selected-frame)) ;; Sets to non-exsistent colour which falls back to terminal bg

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

(after! doom-modeline
  (setq doom-modeline-unicode-fallback nil)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-version nil)
  (setq doom-modeline-height 15))
;; NOTE: toggle with doom/toggle-line-numbers (SPC t l)
(setq display-line-numbers-type nil)

;; Packages
(after! highlight-indent-guides
  (remove-hook! (prog-mode text-mode conf-mode) #'highlight-indent-guides-mode))

(defvar devenv 't) ;; docker, vm (ssh), direnv/nix/localhost (t), devcontainer ?
(cond ((eq devenv 'docker) (setq projectile-project-search-path '(("/docker:de:/src" . 2))))
      (t (setq projectile-project-search-path '(("~/src" . 2)))))
(setq magit-repository-directories projectile-project-search-path)

(map! :leader "p ]" '+ivy/project-search)

(after! spell-fu
  (set-face-attribute 'spell-fu-incorrect-face nil :inherit 'unspecified))

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
  (projectile-register-project-type 'nixflake '("flake.nix")
                                  :compile "darwin-rebuild switch --flake .#"
                                  :run "nix develop"))
(use-package! jsonnet-mode
  :defer t
  :config
  (set-electric! 'jsonnet-mode :chars '(?\n ?: ?} ?}))
  (setq jsonnet-use-smie t))

(after! python
  (add-hook! 'python-mode 'semantic-mode)
  (add-hook! 'python-mode 'semantic-stickyfun-mode))

(after! upload
  (setq ssh-deploy-debug t))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (setq copilot-indent-offset-warning-disable t))

(after! auth-source
  (setq auth-sources (nreverse auth-sources)))

(after! poetry
  (remove-hook 'python-mode-hook #'poetry-tracking-mode))

(load! "+pkm")
