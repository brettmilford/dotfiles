;;; config/scholar/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +scholar/test-create-node ()
  "Test: create a library node and verify it.
Run interactively to verify the data layer works."
  (interactive)
  (let* ((url "https://example.com/test-article")
         (title "Test Article")
         (item-type "article")
         (author "Test Author")
         (file (+scholar--create-node url title item-type "web" author)))
    (if (and file (file-exists-p file))
        (progn
          (find-file file)
          (message "SUCCESS: created %s" file))
      (message "FAIL: node not created"))))

;;;###autoload
(defun +scholar--ensure-dirs ()
  "Ensure library subdirectories exist."
  (dolist (sub '("web" "feed" "docs"))
    (make-directory (expand-file-name sub scholar-library-dir) t)))

;;;###autoload
(defun +scholar--create-node (url title item-type source &optional author)
  "Create a library node for URL with TITLE, ITEM-TYPE, SOURCE, and optional AUTHOR.
Returns the file path of the created node, or nil if URL already exists."
  (+scholar--ensure-dirs)
  (let* ((existing (org-roam-node-from-ref url)))
    (if existing
        (progn
          (message "scholar: node already exists for %s" url)
          (org-roam-node-file existing))
      (let* ((slug (+scholar--slugify title))
             (subdir (pcase source
                       ("rss" "feed")
                       ("web" "web")
                       (_ "docs")))
             (dir (expand-file-name subdir scholar-library-dir))
             (filename (expand-file-name (concat slug ".org") dir))
             (id (org-id-uuid))
             (date (format-time-string "%Y-%m-%d")))
        (with-temp-file filename
          (insert
           ":PROPERTIES:\n"
           (format ":ID:         %s\n" id)
           (format ":ROAM_REFS:  %s\n" url)
           (format ":ITEM_TYPE:  %s\n" item-type)
           (format ":SOURCE:     %s\n" source)
           ":READ_STATE: unread\n"
           (format ":ADDED:      %s\n" date)
           (if author (format ":AUTHOR:     %s\n" author) "")
           ":END:\n"
           (format "#+title: %s\n" title)
           (format "#+filetags: :library:%s:\n\n" item-type)
           "* Content\n\n"
           "* Annotations\n\n"
           "* Discussion\n"))
        (org-roam-db-update-file filename)
        filename))))

;;;###autoload
(defun +scholar--slugify (title)
  "Convert TITLE to a filename-safe slug."
  (let ((slug (downcase title)))
    (setq slug (replace-regexp-in-string "[^a-z0-9]+" "-" slug))
    (setq slug (replace-regexp-in-string "^-+\\|-+$" "" slug))
    (if (> (length slug) 60)
        (substring slug 0 60)
      slug)))
