;;; email/mailroom/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "mu")
  (warn! "mu is not installed. mailroom requires mu for indexing and search."))

(unless (executable-find "mbsync")
  (warn! "mbsync is not installed. mailroom requires mbsync for IMAP sync."))

(unless (executable-find "msmtp")
  (warn! "msmtp is not installed. mailroom requires msmtp for sending mail."))
