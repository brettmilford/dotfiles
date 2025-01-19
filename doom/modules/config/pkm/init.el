;;; config/pkm/init.el -*- lexical-binding: t; -*-

(add-hook! 'doom-after-init-hook :append #'+pkm-eval-config)
