;;; test/test-mailroom-sidebar.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest mailroom-sidebar--extract-account-root-test ()
  "Extract maildir root from mu4e context folder vars."
  (should (equal "/personal"
                 (mailroom-sidebar--extract-account-root
                  '((mu4e-sent-folder . "/personal/Sent")
                    (mu4e-drafts-folder . "/personal/Drafts")
                    (mu4e-trash-folder . "/personal/Trash"))))))

(ert-deftest mailroom-sidebar--build-folder-tree-test ()
  "Build a nested folder tree from flat maildir paths."
  (let ((folders '("/personal/INBOX"
                   "/personal/Sent"
                   "/personal/Projects/client-a"
                   "/personal/Projects/client-b")))
    (let ((tree (mailroom-sidebar--build-folder-tree folders "/personal")))
      ;; Top level should have INBOX, Sent, Projects
      (should (= 3 (length tree)))
      ;; INBOX should be first (sorted)
      (should (equal "INBOX" (plist-get (car tree) :name)))
      ;; Projects should have 2 children
      (let ((projects (cl-find-if (lambda (n) (equal (plist-get n :name) "Projects")) tree)))
        (should projects)
        (should (= 2 (length (plist-get projects :children))))))))
