;; -*- lexical-binding: t; -*-

;; Load custom functions
(add-to-list 'load-path "~/.config/emacs/root-functions")

;; Custom functions
(require 'root-fn-miscellaneous)
(require 'root-fn-aria2)

;; Load modules
(add-to-list 'load-path "~/.config/emacs/root-modules")

(require 'root-theme)
(require 'root-modeline)
(require 'root-package)
(require 'root-base-config)
(require 'root-custom-functions)
(require 'root-keybinds)
(require 'root-dired)

(require 'root-exec-path-from-shell)
(require 'root-undo-fu)
(require 'root-magit)
(require 'root-diff-hl)
(require 'root-vertico)
(require 'root-consult)
(require 'root-org-mode)
(require 'root-openwith)

(require 'root-eglot)
(require 'root-corfu)
(require 'root-prettier)

(require 'root-rjsx)

;; Disable context menu
(context-menu-mode -1)
