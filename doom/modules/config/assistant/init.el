;;; config/assistant/init.el -*- lexical-binding: t; -*-

(add-hook! 'doom-after-init-hook :append #'+assistant-eval-config)
