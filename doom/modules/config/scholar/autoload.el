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

;;;###autoload
(defun +scholar--query-library-nodes (&optional filter limit)
  "Query org-roam DB for library nodes.
FILTER is an optional plist with keys:
  :read-state - string matching READ_STATE property
  :item-type  - string matching ITEM_TYPE property
LIMIT is max results (default 50).
Returns list of plists with :id :title :file :item-type :read-state :added :author."
  (let* ((limit (or limit 50))
         (rows (org-roam-db-query
                [:select [nodes:id nodes:title nodes:file nodes:properties]
                 :from nodes
                 :inner-join tags :on (= nodes:id tags:node-id)
                 :where (= tags:tag "library")])))
    (let ((results
           (mapcar
            (lambda (row)
              (let* ((id (nth 0 row))
                     (title (nth 1 row))
                     (file (nth 2 row))
                     (props (nth 3 row))
                     (read-state (cdr (assoc "READ_STATE" props)))
                     (item-type (cdr (assoc "ITEM_TYPE" props)))
                     (added (cdr (assoc "ADDED" props)))
                     (author (cdr (assoc "AUTHOR" props))))
                (list :id id :title title :file file
                      :item-type item-type :read-state read-state
                      :added added :author author)))
            rows)))
      ;; Apply filters in elisp (properties are not directly queryable in SQL)
      (when-let ((rs (plist-get filter :read-state)))
        (setq results (cl-remove-if-not
                       (lambda (r) (equal (plist-get r :read-state) rs))
                       results)))
      (when-let ((it (plist-get filter :item-type)))
        (setq results (cl-remove-if-not
                       (lambda (r) (equal (plist-get r :item-type) it))
                       results)))
      ;; Apply limit after filtering
      (if (> (length results) limit)
          (seq-take results limit)
        results))))

;;;###autoload
(defun +scholar--query-tags ()
  "Query all tags used across library nodes (excluding 'library' and item-type tags).
Returns alist of (tag . count)."
  (let* ((type-tags '("article" "essay" "paper" "report" "thesis"
                      "book" "feed-entry" "documentation"))
         (rows (org-roam-db-query
                [:select [tags:tag (funcall count tags:tag)]
                 :from tags
                 :inner-join nodes :on (= tags:node-id nodes:id)
                 :where (and (in tags:node-id
                              [:select node-id :from tags
                               :where (= tag "library")])
                             (not-in tags:tag $v1))
                 :group-by tags:tag
                 :order-by [(funcall count tags:tag) :desc]]
                (vconcat (cons "library" type-tags)))))
    (mapcar (lambda (row) (cons (nth 0 row) (nth 1 row))) rows)))

;;;###autoload
(defun +scholar--set-read-state (file state)
  "Set READ_STATE to STATE for the library node at FILE.
STATE is one of: \"unread\", \"reading\", \"read\", \"archived\"."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (org-set-property "READ_STATE" state))
    (save-buffer)))

;;;###autoload
(defun +scholar--fetch-url (url callback)
  "Fetch URL asynchronously, call CALLBACK with (status headers body).
CALLBACK receives three args: status (int), headers (string), body (string)."
  (url-retrieve
   url
   (lambda (status)
     (if-let ((err (plist-get status :error)))
         (funcall callback nil nil nil)
       (goto-char (point-min))
       (let* ((headers (buffer-substring-no-properties
                        (point) (progn (re-search-forward "\n\n" nil t) (point))))
              (body (buffer-substring-no-properties (point) (point-max)))
              (code (when (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" headers)
                      (string-to-number (match-string 1 headers)))))
         (funcall callback code headers body))))
   nil t))

;;;###autoload
(defun +scholar--extract-metadata (html)
  "Extract metadata from HTML string.
Returns plist with :title :author :date :site."
  (let ((title nil) (author nil) (date nil) (site nil))
    ;; og:title or <title>
    (when (string-match "<meta[^>]*property=[\"']og:title[\"'][^>]*content=[\"']\\([^\"']+\\)" html)
      (setq title (match-string 1 html)))
    (unless title
      (when (string-match "<title>\\([^<]+\\)</title>" html)
        (setq title (string-trim (match-string 1 html)))))
    ;; author
    (when (string-match "<meta[^>]*name=[\"']author[\"'][^>]*content=[\"']\\([^\"']+\\)" html)
      (setq author (match-string 1 html)))
    (unless author
      (when (string-match "<meta[^>]*property=[\"']article:author[\"'][^>]*content=[\"']\\([^\"']+\\)" html)
        (setq author (match-string 1 html))))
    ;; date
    (when (string-match "<meta[^>]*property=[\"']article:published_time[\"'][^>]*content=[\"']\\([^\"']+\\)" html)
      (setq date (match-string 1 html)))
    ;; site name
    (when (string-match "<meta[^>]*property=[\"']og:site_name[\"'][^>]*content=[\"']\\([^\"']+\\)" html)
      (setq site (match-string 1 html)))
    (list :title title :author author :date date :site site)))

;;;###autoload
(defun +scholar--html-to-org (html)
  "Convert HTML string to org-mode string via pandoc.
Returns the org content string, or nil if pandoc fails."
  (condition-case err
      (with-temp-buffer
        (insert html)
        (let ((exit-code (call-process-region
                          (point-min) (point-max)
                          "pandoc" t t nil
                          "-f" "html" "-t" "org" "--wrap=none")))
          (if (= exit-code 0)
              (let ((result (string-trim (buffer-string))))
                (if (< (length result) 100)
                    nil  ;; too short, likely garbage
                  result))
            (message "scholar: pandoc exited with code %d" exit-code)
            nil)))
    (file-missing
     (message "scholar: pandoc not found. Install pandoc to enable content extraction.")
     nil)))

;;;###autoload
(defun +scholar--is-pdf-url (url headers)
  "Check if URL points to a PDF based on extension or Content-Type HEADERS."
  (or (string-match-p "\\.pdf\\(?:[?#]\\|$\\)" url)
      (and headers (string-match-p "content-type:.*application/pdf" (downcase headers)))))

;;;###autoload
(defun +scholar/capture-url (url &optional title)
  "Capture URL into the scholar library.
Fetches content, converts to org, creates a library node.
TITLE is optional — extracted from HTML if not provided."
  (interactive "sURL: ")
  (message "scholar: fetching %s..." url)
  (+scholar--fetch-url
   url
   (lambda (code headers body)
     (cond
      ((null code)
       (message "scholar: failed to fetch %s" url))
      ((+scholar--is-pdf-url url headers)
       (message "scholar: URL is a PDF. Use `lit insert --pdf` instead."))
      (t
       ;; Copy data out of url-retrieve buffer before processing
       (let ((fetched-body body)
             (fetched-headers headers))
         (run-at-time 0 nil
          (lambda ()
            (let* ((meta (+scholar--extract-metadata fetched-body))
                   (final-title (or title (plist-get meta :title) "Untitled"))
                   (author (plist-get meta :author))
                   (org-content (+scholar--html-to-org fetched-body))
                   (file (+scholar--create-node url final-title "article" "web" author)))
              (when file
                ;; Insert converted content into the node
                (when org-content
                  (with-current-buffer (find-file-noselect file)
                    (goto-char (point-min))
                    (when (re-search-forward "^\\* Content$" nil t)
                      (forward-line 1)
                      (insert "\n" org-content "\n"))
                    (save-buffer)))
                (message "scholar: captured %s → %s" final-title file)
                (find-file file)))))))))))

;;;###autoload
(defun +scholar--org-protocol-capture (info)
  "Handle org-protocol://scholar-capture?url=URL&title=TITLE.
INFO is the parsed protocol plist."
  (let* ((params (org-protocol-parse-parameters info nil '(:url :title)))
         (url (plist-get params :url))
         (title (plist-get params :title)))
    (when url
      (+scholar/capture-url (org-protocol-sanitize-uri url) title))
    nil))  ;; return nil to prevent org-protocol from opening a file

;;;###autoload
(defun +scholar--format-age (date-string)
  "Format DATE-STRING (YYYY-MM-DD) as relative age (e.g. '2d ago', '1w ago')."
  (if (null date-string)
      ""
    (let* ((then (date-to-time (concat date-string " 00:00:00")))
           (days (/ (float-time (time-subtract (current-time) then)) 86400)))
      (cond
       ((< days 1) "today")
       ((< days 2) "1d ago")
       ((< days 7) (format "%dd ago" (truncate days)))
       ((< days 30) (format "%dw ago" (truncate (/ days 7))))
       ((< days 365) (format "%dmo ago" (truncate (/ days 30))))
       (t (format "%dy ago" (truncate (/ days 365))))))))

;;;###autoload
(defun +scholar--read-state-icon (state)
  "Return display character for read STATE."
  (pcase state
    ("unread"   "o")
    ("reading"  "*")
    ("read"     "x")
    ("archived" "-")
    (_ "?")))

;;;###autoload
(defun +scholar/dashboard ()
  "Open the scholar library dashboard."
  (interactive)
  (let ((buf (get-buffer-create "*scholar*")))
    (with-current-buffer buf
      (+scholar--dashboard-render)
      (switch-to-buffer buf))))

;;;###autoload
(defun +scholar--dashboard-render ()
  "Render the scholar dashboard in current buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (+scholar-dashboard-mode)
    (magit-insert-section (scholar-root)
      ;; Header
      (insert (propertize " scholar" 'face 'magit-section-heading) "  "
              (propertize (abbreviate-file-name scholar-library-dir)
                          'face 'magit-dimmed)
              "\n\n")
      ;; Reading Queue
      (+scholar--dashboard-insert-section
       "Reading Queue"
       (+scholar--query-library-nodes '(:read-state "reading") 20))
      ;; Recently Added
      (+scholar--dashboard-insert-section
       "Recently Added"
       (let ((nodes (+scholar--query-library-nodes nil 20)))
         (sort nodes (lambda (a b)
                       (string> (or (plist-get a :added) "")
                                (or (plist-get b :added) ""))))))
      ;; By Tag
      (+scholar--dashboard-insert-tags)
      ;; Footer
      (insert "\n"
              (propertize " [s]" 'face 'magit-section-heading) " search  "
              (propertize "[a]" 'face 'magit-section-heading) " add URL  "
              (propertize "[f]" 'face 'magit-section-heading) " feeds  "
              (propertize "[r]" 'face 'magit-section-heading) " refresh  "
              (propertize "[q]" 'face 'magit-section-heading) " quit\n"))
    (goto-char (point-min))
    (setq buffer-read-only t)))

;;;###autoload
(defun +scholar--dashboard-insert-section (heading items)
  "Insert a dashboard section with HEADING and ITEMS."
  (when items
    (magit-insert-section (scholar-section heading)
      (magit-insert-heading
        (format "%s (%d)" heading (length items)))
      (dolist (item items)
        (let* ((title (plist-get item :title))
               (item-type (or (plist-get item :item-type) ""))
               (read-state (or (plist-get item :read-state) "unread"))
               (added (plist-get item :added))
               (file (plist-get item :file))
               (icon (+scholar--read-state-icon read-state))
               (age (+scholar--format-age added)))
          (magit-insert-section (scholar-item file t)
            (insert (format "  %s %-50s %-12s %s\n"
                            icon
                            (truncate-string-to-width (or title "") 50 nil nil t)
                            item-type
                            age)))))
      (insert "\n"))))

;;;###autoload
(defun +scholar--dashboard-insert-tags ()
  "Insert the By Tag section in the dashboard."
  (let ((tags (+scholar--query-tags)))
    (when tags
      (magit-insert-section (scholar-section "By Tag")
        (magit-insert-heading "By Tag")
        (dolist (tag-count tags)
          (magit-insert-section (scholar-tag (car tag-count))
            (insert (format "  %-40s (%d)\n"
                            (car tag-count) (cdr tag-count)))))
        (insert "\n")))))

;;;###autoload
(defun +scholar/dashboard-refresh ()
  "Refresh the scholar dashboard."
  (interactive)
  (when (equal (buffer-name) "*scholar*")
    (+scholar--dashboard-render)))

;;;###autoload
(defun +scholar/dashboard-open-item ()
  "Open the library item at point."
  (interactive)
  (when-let ((section (magit-current-section))
             (value (oref section value)))
    (when (stringp value)
      (find-file value))))

;;;###autoload
(defun +scholar/dashboard-add-url ()
  "Prompt for a URL and capture it."
  (interactive)
  (let ((url (read-string "URL: ")))
    (when (and url (not (string-empty-p url)))
      (+scholar/capture-url url))))

;;;###autoload
(defun +scholar/search-library ()
  "Search library nodes with consult-ripgrep."
  (interactive)
  (consult-ripgrep scholar-library-dir))
