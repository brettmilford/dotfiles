;;; config/pkm/config.el -*- lexical-binding: t; -*-
;;; --- Personal Knowledge Management config

(defun +pkm-load ()
  (setq org-roam-directory org-directory)
  (add-to-list 'auto-mode-alist '("\\.org_archive\\'" . org-mode))
  (add-to-list 'auto-mode-alist '("\\.org.gpg\\'"     . org-mode))
  (defvar +pkm-org-ext ".org")
  (defvar +pkm-org-archive-ext ".org_archive")
  (remove-hook 'text-mode-hook #'vi-tilde-fringe-mode)
  (require 'org-crypt)
  (setq epa-file-encrypt-to `(,user-mail-address)
        epa-file-select-keys 1
        org-crypt-key user-mail-address
        org-crypt-disable-auto-save t)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt")))

(defun +pkm-encrypt()
  (add-to-list 'auto-mode-alist '("\\.org.gpg\\'" . org-mode))
  (add-to-list 'auto-mode-alist '("\\.org_archive.gpg\\'" . org-mode))
  (setq epa-file-encrypt-to `(,user-mail-address)
        epa-file-select-keys 1
        org-crypt-key user-mail-address
        org-crypt-disable-auto-save t)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq +pkm-org-ext ".org.gpg"
        +pkm-org-archive-ext ".org_archive.gpg")
  (setq org-archive-location "archive.org.gpg::* From %s")
  (defun +pkm/open-all-project-files ()
    "Open all project files"
    (interactive)
    (let ((project-root (doom-modeline--project-root)))
      (when project-root
        (do-list (file (projectile-current-projhect-files))
                 (when (string-suffix-p +pkm-org-ext file)
                   (find-file-noselect (expand-file-name file project-root)))))))

  (defun +pkm/open-all-project-files-alt ()
    "Open all project files"
    (interactive)
    (let ((project-root (doom-modeline--project-root)))
          (when project-root
            (let ((files (directory-files-recursively project-root +pkm-org-ext)))
              (when files
                (dolist (file files)
                  (find-file-noselect file)))))))

  (cond ((modulep! :completion ivy)
         (advice-add 'swiper-all :before #'pkm/open-all-project-files-alt))
        ((modulep! :completion vertico)
         (map! :leader
               :prefix ("s" . "search")
               :desc "Search all open buffers" "B"
               (cmd! (progn
                       (+pkm/open-all-project-files-alt)
                       (consult-line-multi 'all-buffers))))))
  (after! org
    (unless (string-match-p "\\.gpg" org-agenda-file-regexp)
      (setq org-agenda-file-regexp
            (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                      org-agenda-file-regexp)))))
(defun +pkm-org-appeareance-x ()
  (use-package! org-modern
      :hook ((org-mode . org-modern-mode)
             (org-agenda-finalize . org-modern-agenda))
      :config
      (setq
       org-auto-align-tags nil
       org-catch-invisible-edits 'show-and-error
       org-special-ctrl-a/e t
       org-insert-heading-respect-content t
       org-hide-emphasis-markers t
       org-ellipsis "…"
       org-agenda-tags-column 0
       org-agenda-block-separator ?─
       org-agenda-time-grid
       '((daily today require-timed)
         (800 1000 1200 1400 1600 1800 2000)
         " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
       org-agenda-current-time-string
       "◀── now ─────────────────────────────────────────────────"
       org-modern-todo-faces
            '(("WAIT" . (:inherit +org-todo-onhold :inverse-video t))
              ("HOLD" . (:inherit +org-todo-onhold :inverse-video t)))
            org-modern-priority-faces
            '((?A . (:inherit error :inverse-video t))
              (?B . (:inherit warning :inverse-video t))))
      (set-face-attribute 'org-modern-symbol nil :family "Iosevka"))
  (setq writeroom-fringes-outside-margins nil)
  (setq +zen-text-scale 0)
  (add-hook! 'org-mode-hook #'writeroom-mode))

(defun +pkm-org ()
  ;; Stop this from overriding my global C-k binding
  (map! :after org
        :map org-mode-map
        :n "C-k" nil)
  (setq display-line-numbers-type nil)
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-mouse) ;; may need to require instead
  (org-clock-persistence-insinuate)

  ;(add-hook! 'org-mode-hook #'+word-wrap-mode)
  (remove-hook 'org-mode-hook #'auto-fill-mode)

  (map!
   :leader
   :desc "Org Agenda" "A" #'(lambda () (interactive) (org-agenda nil "n")))

  (map!
   "C-x S" 'org-save-all-org-buffers
   :map org-mode-map
   :localleader
     "TAB" #'org-insert-structure-template
     "y" #'+pkm/org-yank-block-content
     :prefix ("c" . "clock")
       :desc "Capture clocked" "x" #'+pkm/org-capture-clocked)

  (defun +pkm/org-capture-clocked ()
    (interactive)
    (let ((org-capture-templates '(("c" "clocked" entry (clock) "* %?\n%i\n%a"))))
      (+org-capture/open-frame nil "c")))

  (defun +pkm/org-yank-block-content ()
    "Copy everything in #+ blocks"
    (interactive)
    (save-restriction
        (org-narrow-to-block)
        (let ((content (buffer-substring-no-properties (point-min) (point-max))))
          (with-temp-buffer
            (insert content)
            (goto-char (point-min))
            (while (re-search-forward "^#\\+.*\n?" nil t)
              (replace-match ""))
            (kill-ring-save (point-min) (point-max))))
          (widen))
    (message "Block yanked."))
  ;(setq org-capture-templates
  ;      '(("t" "Todo" entry
  ;         (file+headline org-default-notes-file "Inbox")
  ;         "* TODO %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+1d\"))\n%i\n%a")))

  (setq org-todo-keywords
        '((sequence
           "TODO(t!)"
           "INPROGRESS(i!)"
           "WAIT(w@/!)"
           "HOLD(h@/!)"
           "|"
           "DONE(d)"
           "CANCELLED(c)"))
        org-todo-keyword-faces
        '(("INPROGRESS" . +org-todo-active)
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("CANCELLED" . +org-todo-cancel)))

  (setq +org-capture-todo-file (concat "todo" +pkm-org-ext)
        +org-capture-notes-file (concat "notes" +pkm-org-ext))
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Backlog")
           "* TODO %?\n%i\n%a" :prepend t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t)

          ("p" "Templates for projects")
          ("pt" "Project-local todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Backlog")
           "* TODO %?\n%i\n%a" :prepend t)
          ("pn" "Project-local notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("pc" "Project-local changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-changelog-file "Unreleased")
           "* %U %?\n%i\n%a" :prepend t)

          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           (function +pkm/org-capture-central-project-file)
           "* TODO %?\n%i\n %a"
           :heading "Backlog"
           :prepend nil)
          ("on" "Project notes" entry
           (function +pkm/org-capture-central-project-file)
           "* %U %?\n %i\n %a"
           :heading "Notes"
           :prepend t)
          ("oc" "Project changelog" entry
           (function +pkm/org-capture-central-project-file)
           "* %U %?\n %i\n %a"
           :heading "Changelog"
           :prepend t)))

(defun +pkm/org-capture-central-project-file ()
  "TODO"
  (let ((project-name (projectile-project-name)))
    (+org--capture-central-file
      (concat "projects/" project-name ".org") project-name)))

  (let ((project-dir (expand-file-name "projects/" org-directory)))
    (if (file-directory-p project-dir)
        (setq org-agenda-files (append (list org-directory)
                                       (directory-files project-dir t org-agenda-file-regexp)))
      (setq org-agenda-files (list org-directory))))
  (setq
  org-agenda-window-setup 'reorganize-frame
  org-habit-show-habits t
  org-habit-show-all-today t ;; BUG: No habit entries are shown otherwise?
  org-habit-show-habits-only-for-today t
  org-mouse-1-follows-link 'double
  org-columns-default-format "%25ITEM %3PRIORITY %TODO %SCHEDULED %DEADLINE %TAGS"
  org-fontify-done-headline t
  org-agenda-view-columns-initially nil
  org-refile-targets '((nil :maxlevel . 3)
                       (org-agenda-files :maxlevel . 3))
  org-refile-use-cache nil
  org-refile-target-verify-function
  (lambda ()
   "Filters out Archive nodes"
   (if (string= (nth 4 (org-heading-components)) "Archive")
           (unless (ignore-errors (org-forward-element))
             (goto-char (point-max))) t))
  org-refile-allow-creating-parent-nodes 'confirm
  org-refile-use-outline-path 'file
  org-reverse-note-order t
  org-outline-path-complete-in-steps nil
  org-startup-folded t
  org-cycle-open-archived-trees t
  org-adapt-indentation nil
  org-log-done 'time
  org-enforce-todo-dependencies t
  org-latex-bib-compiler "biber"
  org-latex-pdf-process
  '("%latex -interaction nonstopmode -output-directory %o %f"
    "%bib %b"
    "%latex -interaction nonstopmode -output-directory %o %f"
    "%latex -interaction nonstopmode -output-directory %o %f")
  org-export-date-timestamp-format "%B %-e, %Y"
  org-log-into-drawer t
  org-table-duration-custom-format 'minutes
  org-clock-persist t
  org-clock-continuously nil ;; TODO: Check shouldn't be 't
  org-clock-persist-query-resume nil
  org-clock-out-when-done t
  org-clock-report-include-clocking-task t
  org-html-self-link-headlines t
  org-use-tag-inheritance t
  org-crypt-key "brettmilford@gmail.com"
  org-startup-indented nil
  org-tags-column 0
  org-pretty-entities t)

  (use-package! ox-reveal
    :after org-mode)

  (load! "lisp/org-notification"))

(defun +pkm-org-roam ()
  ;; BUG: org-roam/pull/2141
  (map!
   :leader
   :prefix ("n" . "notes")
   (:prefix ("r" . "org-roam")
    :desc "Tag file" "t" #'org-roam-tag-add ;; NOTE: 'SPC m m o t' as well.
    :desc "Insert node w/ template" "I" #'(lambda () (interactive) (+pkm-org-roam-tmpl-capture 'org-roam-node-insert "./roam/tmpl"))
    :desc "Find node w/ template" "N" #'(lambda () (interactive) (+pkm-org-roam-tmpl-capture 'org-roam-node-find "./roam/tmpl"))
    (:prefix ("d" . "by date")
     :desc "Goto date" "d" #'(lambda () (interactive) (org-roam-dailies-goto-date nil "d"))
     :desc "Goto tomorrow" "m" #'(lambda () (interactive) (org-roam-dailies-goto-tomorrow nil "d"))
     :desc "Goto today" "n" #'(lambda () (interactive) (org-roam-dailies-goto-today "d"))
     :desc "Goto yesterday" "y" #'(lambda () (interactive) (org-roam-dailies-goto-yesterday nil "d"))
     :desc "Goto date w/ template" "x" #'+pkm-org-roam-dailies-capture))
   :map org-mode-map
   :localleader
   :prefix ("m" . "org-roam")
   (:prefix ("d" . "by date")
    :desc "Goto date" "d" #'(lambda () (interactive) (org-roam-dailies-goto-date nil "d"))
    :desc "Goto tomorrow" "m" #'(lambda () (interactive) (org-roam-dailies-goto-tomorrow nil "d"))
    :desc "Goto today" "n" #'(lambda () (interactive) (org-roam-dailies-goto-today "d"))
    :desc "Goto yesterday" "y" #'(lambda () (interactive) (org-roam-dailies-goto-yesterday nil "d"))
    :desc "Capture template today" "x" #'org-roam-dailies-capture-today-w-tmpl))

  ;; makes id links work, if org-mode hasn't cached them
  ;; TODO: kills startup performance.
  ;; (org-id-update-id-locations (org-roam-list-files) 't)

  ;; Exclude archive notes from org-roam parsing
  (add-to-list 'org-roam-file-exclude-regexp (concat org-directory "/archive/"))

  (setq org-roam-mode-sections
        '((org-roam-backlinks-section :unique t)
           org-roam-reflinks-section)
        org-roam-buffer-no-delete-other-windows 't
        org-roam-completion-system 'ivy
        org-roam-db-gc-threshold most-positive-fixnum
        ;org-roamtag-sources '(prop all-directories)
        )

  (setq org-roam-capture-templates
        `(("d" "default" plain
          "* ${title}\n%?"
          :target (file+head ,(concat "./roam/${cxt}/%<%Y%m%d%H%M%S>-${slug}" +pkm-org-ext) "#+title: ${title}\n- topics ::\n")
          :unnarrowed t)))

  (defun +pkm-org-roam-tmpl-capture (fun dir)
    (let* ((tmpl-dir (expand-file-name dir org-roam-directory))
           (files (directory-files-recursively tmpl-dir "" t))
           (org-roam-dailies-capture-templates
            (mapcar (lambda (file)
                 (let* ((f (file-name-nondirectory file))
                        (key (replace-regexp-in-string "_.*" "" f))
                        (desc (replace-regexp-in-string "\\(^.*_\\|\.org$\\)" "" f)))
                   (if (file-directory-p file)
                     `(,key ,desc)
                     `(,key ,desc plain
                       (file ,file)
                       :if-new (file ,(concat "./roam/%<%Y%m%d%H%M%S>-${slug}" +pkm-org-ext))))))
               files)))
    (funcall fun nil :templates org-roam-dailies-capture-templates)))

  (setq org-roam-dailies-capture-templates
        `(("d" "default" entry "* %? :crypt:\n%U\n"
           :if-new (file+head ,(concat "%<%Y-%m-%d>" +pkm-org-ext)
                              "#+title: %<%A the %-e of %B %Y>\n#+filetags: %<:%Y:%B:>\n")
           :unnarrowed t)))

  (defun +pkm-org-roam-dailies-capture ()
    (interactive)
    (let* ((tmpl-dir (expand-file-name "./daily/tmpl" org-roam-directory))
           (files (directory-files-recursively tmpl-dir "" t))
           (org-roam-dailies-capture-templates
            (mapcar (lambda (file)
                 (let* ((f (file-name-nondirectory file))
                        (key (replace-regexp-in-string "_.*" "" f))
                        (desc (replace-regexp-in-string "\\(^.*_\\|\.org$\\)" "" f)))
                   (if (file-directory-p file)
                     `(,key ,desc)
                     `(,key ,desc entry
                       (file ,file)
                       :if-new (file+head ,(concat "%<%Y-%m-%d>" +pkm-org-ext)
                                          "#+title: %<%A the %-e of %B %Y>\n#+filetags: %<:%Y:%B:>\n\n")))))
               files)))
      (org-roam-dailies-capture-date)))

(after! org-roam-graph
  (if IS-MAC
   (setq org-roam-graph-viewer "open")
   (setq org-roam-graph-viewer "xdg-open")))

(use-package! websocket
    :after org-roam-ui)

(use-package! org-roam-ui
    :after org-roam
    :config
    (map!
     :leader
     (:prefix ("n" . "notes")
     (:prefix ("r" . "roam")
      :desc "Org Roam UI" "u"
      (lambda () (interactive)
        (if (member '(org-roam-ui-mode " org-roam-ui") minor-mode-alist)
            (org-roam-ui-open)
          (org-roam-ui-mode))))))

    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t)))

(defun +pkm-org-cite ()
  (setq reftex-default-bibliography (expand-file-name "references.bib" org-directory))
  (setq org-cite-global-bibliography (list reftex-default-bibliography))
  (after! bibtex
    (setq bibtex-autokey-year-length 4
          bibtex-autokey-name-year-separator ""
          bibtex-autokey-year-title-separator ""
          bibtex-autokey-titleword-separator ""
          bibtex-autokey-titlewords 1
          bibtex-autokey-titlewords-stretch 1
          bibtex-autokey-titleword-length nil))

  (after! bibtex-completion
    (advice-add 'bibtex-completion-candidates
                :filter-return 'reverse)
    (setq bibtex-completion-notes-path (file-name-as-directory (expand-file-name "annotations" org-directory))
          bibtex-completion-library-path (file-name-as-directory (expand-file-name "fulltext" org-directory))
          bibtex-completion-bibliography reftex-default-bibliography)
    (setq bibtex-completion-notes-template-multiple-files
          (concat
           ":PROPERTIES:\n"
           ":ROAM_ALIASES: ${=key=}\n"
           ":ROAM_REFS: @${=key=}\n"
           ":END:\n"
           "#+title: ${title}\n"
           "#+filetags: ${keywords}\n\n"
           "- topics :: \n"
           "* ${title}\n"
           ":PROPERTIES:\n"
           ":Custom_ID: ${=key=}\n"
           ":URL: ${url}\n"
           ":AUTHOR: ${author-abbrev}\n"
           ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
           ":NOTER_PAGE:\n"
           ":JOURNAL: ${journaltitle}\n"
           ":DATE: ${date}\n"
           ":YEAR: ${year}\n"
           ":DOI: ${doi}\n"
           ":END:\n\n"))
    (add-hook 'bibtex-completion-notes-mode-hook #'org-id-get-create))

  ;; TODO: Switch to citar
  (use-package! org-ref
    :commands (org-ref-insert-cite-link org-ref-citation-hydra/body org-ref-bibtex-hydra/body)
    :config
    (map!
     (:map 'bibtex-mode-map
           "C-c C-o" #'org-ref-bibtex-hydra/body))
    (setq org-ref-insert-cite-function
          (lambda ()
            (org-cite-insert nil))))

  (use-package! org-roam-bibtex
    :after org-roam
    :config
    (setq orb-preformat-keywords
     '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
    (setq orb-process-file-field t)
    (setq orb-insert-link-description 'citation-org-cite)
    (setq orb-citekey-format 'org-cite)
    (map!
     :leader
     :prefix ("n" . "notes")
     (:prefix ("r" . "roam")
      :desc "ORB Edit Citation Note" "c" #'orb-edit-citation-note
      :desc "ORB Note Actions" "b" #'orb-note-actions
      :desc "ORB Insert Link" "@" #'orb-insert-link))
    (defun orb-capture-template (oldfun citekey &rest args)
      "Bind org-roam-capture-templates for orb."
      (let ((org-roam-capture-templates
             `(("r" "bibliography reference" plain "%?\n"
                :target (file+head "annotations/${citekey}.org"
                                   ,(concat
                                    "#+title: ${title}\n"
                                    "#+filetags: ${keywords}\n"
                                    "- topics ::\n"
                                    "* ${title}\n"
                                    ":PROPERTIES:\n"
                                    ":Custom_ID: ${citekey}\n"
                                    ":URL: ${url}\n"
                                    ":AUTHOR: ${author-or-editor}\n"
                                    ":NOTER_DOCUMENT: ${file}\n"
                                    ":NOTER_PAGE:\n"
                                    ":END:\n"
                                    ))
                :unnarrowed t))))
        (apply oldfun citekey args)))
    (advice-add 'orb--new-note :around 'orb-capture-template)))

(defun +pkm-org-reading ()
  (after! org-noter
    (setq org-noter-notes-search-path bibtex-completion-notes-path))

  (use-package! nov
    :init
    (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

  (use-package! anki-editor
    :config
    (setq anki-editor-create-decks t))

  (use-package! org-mode-incremental-reading
    :hook (incremental-reading-mode . anki-editor-mode)
    :config
    (defun org-protocol-open-file (fname)
      "Process an org-protocol://open-file?url= style URL with FNAME.
      Change a filename by mapping URLs to local filenames as set
      in `org-protocol-project-alist'.
      The location for a browser's bookmark should look like this:
      javascript:location.href = \\='org-protocol://open-file?url=\\=' + \\
      encodeURIComponent(location.href)"
      (let ((f (org-protocol-sanitize-uri
                (plist-get (org-protocol-parse-parameters fname nil '(:file))
                           :file))))
        f))

    (add-to-list
     'org-protocol-protocol-alist
     '("org-open-file" :protocol "open-file" :function org-protocol-open-file))))

(defun +pkm-org-link-ghe ()
  (org-link-set-parameters "ghe"
                           :follow (lambda (path)
                                     (let* ((org (car (split-string path "/")))
                                            (repo (cadr (split-string path "/")))
                                            (issue (caddr (split-string path "/"))))
                                       (browse-url (format "https://git/%s/%s/issues/%s" org repo issue))))
                           :export (lambda (path desc backend)
                                     (let* ((org (car (split-string path "/")))
                                            (repo (cadr (split-string path "/")))
                                            (issue (caddr (split-string path "/"))))
                                       (cond
                                        ((eq backend 'html)
                                         (format "<a href='https://git/%s/%s/issues/%s'>%s</a>" org repo issue desc))
                                        ((eq backend 'latex)
                                         (format "\\href{https://git/%s/%s/issues/%s}{%s}" org repo issues desc))
                                        ((eq backend 'ascii)
                                         (format "https://git/%s/%s/issues/%s" org repo issue))
                                        ((eq backend 'md)
                                         (format "[%s](https://git/%s/%s/issues/%s)" desc org repo issue)))))))

(defun +pkm-org-capture-template-jira ()
  (pushnew! org-link-abbrev-alist '("jira" .  "https://jira/browse/%s"))

  (defun jira-capture-enrichment ()
    (when-let* ((pt (point))
                (issue-key (and (org-at-heading-p)
                                (org-entry-get pt "JIRAISSUEKEY"))))
      (let-alist (jiralib2-get-issue issue-key)
        ;; Update headline
        (let ((headline (format "[[jira:%s][%s]] %s" .key .key .fields.summary)))
          (message "Updating headline to : %s" headline)
          (org-edit-headline headline))
        ;; Update properties
        (message "Updating Property Drawer")
        (cl-loop
         for (property value)
         on (list
             "JiraAssignee" .fields.assignee.displayName
             "JiraCreate" .fields.created
             "JiraIssueKey" .key
             "JiraIssueType" .fields.issuetype.name
             "JiraPriority" .fields.priority.name
             "JiraProjectKey" .fields.project.key
             "JiraReporter" .fields.reporter.displayName
             "JiraStatus" .fields.status.name
             "JiraSummary" .fields.summary)
         by #'cddr
         do (org-entry-put pt property value))
        (message "Inserting org-roam-node")
        (org-end-of-subtree)
        (insert "\n")
        (let* ((node (org-roam-node-create :title .key))
               (id (org-roam-node-id node)))
          (org-roam-capture-
           :node node
           :keys "d"
           :templates `(("d" "default" plain "* {title} Notes"
                         :target (file+head ,(concat "./jira/${slug}.org" +pkm-org-ext)
                                            "#+title: [[jira:${title}][${title}]]\n")))
           :props (append
                   (list :link-description (format "%s Notes" .key)
                         :jump-to-captured nil
                         :immediate-finish t
                         :finalize 'insert-link)))))))

  (add-to-list 'org-capture-templates
               '("i" "Jira Issue" entry
                 (file+headline +org-capture-todo-file "Backlog")
                 "* TODO %^{JiraIssueKey}p\n%i\n%a"
                 :immediate-finish t
                 :jump-to-captured t
                 :empty-lines-after 1
                 :hook jira-capture-enrichment
                 :prepend t)))


(defun +pkm-eval-config ()
  (setq org-directory "~/org")
  (defvar +pkm-encrypt-enabled nil)
  (+pkm-load)
  (when (modulep! +encrypt)
      (setq +pkm-encrypt-enabled 't)
      (+pkm-encrypt))

  (after! org
    (+pkm-org)
    (when (modulep! +jira)
      (+pkm-org-link-ghe)
      (+pkm-org-capture-template-jira))
    (when window-system
      (+pkm-org-appeareance-x)))

  (after! org-roam
    (+pkm-org-roam))

  (+pkm-org-cite)
  (+pkm-org-reading))
