;;; email/mailroom/autoload/mailroom-sidebar.el -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'cl-lib)

;;; Data Model

;;;###autoload
(defun mailroom-sidebar--extract-account-root (vars)
  "Extract the maildir root directory from context VARS.
VARS is an alist of mu4e variables. Finds the common prefix
among folder variables like mu4e-sent-folder, mu4e-drafts-folder, etc."
  (let* ((folder-keys '(mu4e-sent-folder mu4e-drafts-folder
                        mu4e-trash-folder mu4e-refile-folder))
         (folders (cl-loop for key in folder-keys
                           for val = (alist-get key vars)
                           when val collect val))
         (first (car folders)))
    (when first
      ;; Extract "/<account>" from "/<account>/Subfolder"
      (if (string-match "^\\(/[^/]+\\)/" first)
          (match-string 1 first)
        first))))

;;;###autoload
(defun mailroom-sidebar--build-folder-tree (folders root)
  "Build a nested tree structure from flat FOLDERS list.
ROOT is the account maildir root (e.g. \"/personal\").
Returns a list of plists with :name, :path, and :children keys."
  (let ((stripped (mapcar (lambda (f)
                           (string-remove-prefix (concat root "/") f))
                         folders))
        (top-level (make-hash-table :test 'equal)))
    ;; Group by first path component
    (dolist (path stripped)
      (let* ((parts (split-string path "/"))
             (head (car parts))
             (rest (string-join (cdr parts) "/")))
        (unless (gethash head top-level)
          (puthash head nil top-level))
        (when (and rest (not (string-empty-p rest)))
          (puthash head (cons (concat root "/" head "/" rest)
                              (gethash head top-level))
                   top-level))))
    ;; Build tree recursively
    (let (result)
      (maphash (lambda (name children)
                 (push (list :name name
                             :path (concat root "/" name)
                             :children (when children
                                         (mailroom-sidebar--build-folder-tree
                                          (nreverse children)
                                          (concat root "/" name))))
                       result))
               top-level)
      ;; Sort: INBOX first, then alphabetical
      (sort result (lambda (a b)
                     (let ((na (plist-get a :name))
                           (nb (plist-get b :name)))
                       (cond ((equal na "INBOX") t)
                             ((equal nb "INBOX") nil)
                             (t (string< na nb)))))))))

;;;###autoload
(defun mailroom-sidebar--get-accounts ()
  "Get all email accounts from mu4e-contexts.
Returns a list of plists with :name, :email, :root, and :context keys."
  (cl-loop for ctx in mu4e-contexts
           for name = (mu4e-context-name ctx)
           for vars = (mu4e-context-vars ctx)
           for email = (alist-get 'user-mail-address vars)
           for root = (mailroom-sidebar--extract-account-root vars)
           when root
           collect (list :name name
                         :email email
                         :root root
                         :context ctx)))

;;;###autoload
(defun mailroom-sidebar--find-maildirs (root)
  "Find all maildirs under ROOT by looking for cur/new/tmp subdirectories.
ROOT is relative to `mu4e-maildir' (e.g. \"/personal\")."
  (let* ((absolute-root (expand-file-name (substring root 1)
                                          (or (bound-and-true-p mu4e-maildir)
                                              (expand-file-name "~/.mail"))))
         (maildirs '()))
    (when (file-directory-p absolute-root)
      (dolist (dir (directory-files-recursively absolute-root "^cur$" t))
        (let* ((maildir-abs (file-name-directory (directory-file-name dir)))
               (maildir-rel (concat root "/" (file-relative-name
                                              maildir-abs absolute-root))))
          ;; Remove trailing slash
          (setq maildir-rel (directory-file-name maildir-rel))
          ;; Skip the root itself if it's a maildir
          (unless (equal maildir-rel root)
            (push maildir-rel maildirs)))))
    (sort maildirs #'string<)))

;;;###autoload
(defun mailroom-sidebar--unread-count (maildir-path)
  "Get unread message count for MAILDIR-PATH using mu.
MAILDIR-PATH is like \"/personal/INBOX\"."
  (let ((output (shell-command-to-string
                 (format "mu find maildir:%s flag:unread 2>/dev/null | wc -l"
                         (shell-quote-argument maildir-path)))))
    (string-to-number (string-trim output))))

;;;###autoload
(defun mailroom-sidebar--unread-counts-batch (maildir-paths)
  "Get unread counts for all MAILDIR-PATHS in a single mu query.
Returns an alist of (path . count)."
  (let ((counts '()))
    (dolist (path maildir-paths)
      (push (cons path (mailroom-sidebar--unread-count path)) counts))
    (nreverse counts)))

;;; Rendering

(defvar mailroom-sidebar-buffer-name "*mailroom-sidebar*"
  "Name of the sidebar buffer.")

(defvar mailroom-sidebar--collapsed-accounts nil
  "List of account names whose folder trees are collapsed.")

(defvar-local mailroom-sidebar--folder-at-point-alist nil
  "Alist mapping line numbers to folder plists for navigation.")

(defface mailroom-sidebar-account-face
  '((t :inherit (bold font-lock-keyword-face)))
  "Face for account names in the sidebar.")

(defface mailroom-sidebar-folder-face
  '((t :inherit default))
  "Face for folder names in the sidebar.")

(defface mailroom-sidebar-unread-face
  '((t :inherit (bold font-lock-constant-face)))
  "Face for folders with unread messages.")

(defface mailroom-sidebar-count-face
  '((t :inherit font-lock-comment-face))
  "Face for unread count numbers.")

(defface mailroom-sidebar-search-heading-face
  '((t :inherit (bold font-lock-type-face)))
  "Face for the Saved Searches heading.")

(defface mailroom-sidebar-search-face
  '((t :inherit font-lock-string-face))
  "Face for saved search entries.")

;;;###autoload
(define-derived-mode mailroom-sidebar-mode special-mode "Mailroom-Sidebar"
  "Major mode for the mailroom sidebar buffer.
Displays all email accounts and their folders."
  :group 'mailroom
  (setq buffer-read-only t
        truncate-lines t
        cursor-type nil)
  (buffer-disable-undo))

;;;###autoload
(defun mailroom-sidebar--render ()
  "Render the sidebar buffer with all accounts and folders."
  (let ((accounts (mailroom-sidebar--get-accounts))
        (inhibit-read-only t)
        (line-map '())
        (line 1))
    (with-current-buffer (get-buffer-create mailroom-sidebar-buffer-name)
      (erase-buffer)
      (mailroom-sidebar-mode)
      ;; Render accounts
      (dolist (account accounts)
        (let* ((name (plist-get account :name))
               (email (plist-get account :email))
               (root (plist-get account :root))
               (collapsed-p (member name mailroom-sidebar--collapsed-accounts))
               (maildirs (mailroom-sidebar--find-maildirs root))
               (tree (mailroom-sidebar--build-folder-tree maildirs root))
               (indicator (if collapsed-p "▶ " "▼ ")))
          ;; Account header line
          (insert (propertize (concat indicator name)
                              'face 'mailroom-sidebar-account-face
                              'mailroom-account name)
                  (if email
                      (propertize (format " (%s)" email) 'face 'font-lock-comment-face)
                    "")
                  "\n")
          (push (cons line (list :type 'account :name name)) line-map)
          (cl-incf line)
          ;; Render folder tree (unless collapsed)
          (unless collapsed-p
            (let ((line-state (cons line-map line)))
              (mailroom-sidebar--render-tree tree 1 line-state)
              (setq line-map (car line-state)
                    line (cdr line-state))))))
      ;; Separator
      (insert (propertize (make-string (or (bound-and-true-p mailroom-sidebar-width) 30) ?─)
                          'face 'font-lock-comment-face)
              "\n")
      (cl-incf line)
      ;; Saved searches
      (when (bound-and-true-p mailroom-saved-searches)
        (insert (propertize "Saved Searches"
                            'face 'mailroom-sidebar-search-heading-face)
                "\n")
        (cl-incf line)
        (dolist (search mailroom-saved-searches)
          (let ((name (plist-get search :name)))
            (insert (propertize (concat "  " name)
                                'face 'mailroom-sidebar-search-face)
                    "\n")
            (push (cons line (list :type 'search
                                   :name name
                                   :query (plist-get search :query)))
                  line-map)
            (cl-incf line))))
      (setq mailroom-sidebar--folder-at-point-alist (nreverse line-map))
      (goto-char (point-min)))))

;;;###autoload
(defun mailroom-sidebar--render-tree (nodes depth line-state)
  "Render folder tree NODES at indentation DEPTH.
LINE-STATE is a cons of (line-map . current-line-number).
Mutates LINE-STATE in place."
  (let ((indent (make-string (* depth 2) ?\s))
        (line-map (car line-state))
        (line (cdr line-state)))
    (dolist (node nodes)
      (let* ((name (plist-get node :name))
             (path (plist-get node :path))
             (children (plist-get node :children))
             (count (mailroom-sidebar--unread-count path))
             (has-unread (> count 0))
             (face (if has-unread 'mailroom-sidebar-unread-face
                     'mailroom-sidebar-folder-face)))
        (insert (propertize (concat indent name) 'face face))
        (when has-unread
          (insert (propertize (format " (%d)" count)
                              'face 'mailroom-sidebar-count-face)))
        (insert "\n")
        (push (cons line (list :type 'folder :name name :path path)) line-map)
        (cl-incf line)
        ;; Recurse into children
        (when children
          (setcar line-state line-map)
          (setcdr line-state line)
          (mailroom-sidebar--render-tree children (1+ depth) line-state)
          (setq line-map (car line-state)
                line (cdr line-state)))))
    (setcar line-state line-map)
    (setcdr line-state line)))

;;;###autoload
(defun mailroom-sidebar-open-at-point ()
  "Open the folder or search at point in the message list pane."
  (interactive)
  (let* ((line (line-number-at-pos))
         (entry (alist-get line mailroom-sidebar--folder-at-point-alist)))
    (when entry
      (let ((type (plist-get entry :type)))
        (cond
         ((eq type 'folder)
          (mailroom-layout--focus-pane 'list)
          (mu4e-headers-search (format "maildir:%s" (plist-get entry :path))))
         ((eq type 'search)
          (mailroom-layout--focus-pane 'list)
          (mu4e-headers-search (plist-get entry :query)))
         ((eq type 'account)
          (mailroom-sidebar-toggle-account)))))))

;;;###autoload
(defun mailroom-sidebar-toggle-account ()
  "Toggle collapse/expand of the account at point."
  (interactive)
  (let* ((line (line-number-at-pos))
         (entry (alist-get line mailroom-sidebar--folder-at-point-alist)))
    (when (and entry (eq (plist-get entry :type) 'account))
      (let ((name (plist-get entry :name)))
        (if (member name mailroom-sidebar--collapsed-accounts)
            (setq mailroom-sidebar--collapsed-accounts
                  (delete name mailroom-sidebar--collapsed-accounts))
          (push name mailroom-sidebar--collapsed-accounts))
        (mailroom-sidebar--render)))))

;;;###autoload
(defun mailroom-sidebar-next-account ()
  "Jump to the next account heading in the sidebar."
  (interactive)
  (let ((current-line (line-number-at-pos))
        (target nil))
    (dolist (entry mailroom-sidebar--folder-at-point-alist)
      (when (and (eq (plist-get (cdr entry) :type) 'account)
                 (> (car entry) current-line)
                 (or (null target) (< (car entry) (car target))))
        (setq target entry)))
    ;; Wrap around to first account if at bottom
    (unless target
      (setq target (cl-find-if (lambda (e) (eq (plist-get (cdr e) :type) 'account))
                               mailroom-sidebar--folder-at-point-alist)))
    (when target
      (goto-char (point-min))
      (forward-line (1- (car target))))))
