;;; config/private/init.el -*- lexical-binding: t; -*-

(add-hook! 'doom-after-init-hook :append #'+private-eval-config)
