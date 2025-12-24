;; -*- lexical-binding: t; -*-

;; Load custom functions
(add-to-list 'load-path "~/.config/emacs/root-functions")

;; Custom functions
(require 'root-fn-miscellaneous)
(require 'root-fn-aria2)
(require 'root-fn-media-playback)

;; Load modules
(add-to-list 'load-path "~/.config/emacs/root-modules")

;; Modules with no external packages
(require 'root-theme)
(require 'root-modeline)
(require 'root-package)
(require 'root-base-config)
(require 'root-keybinds)
(require 'root-dired)

;; Modues with external packages
(require 'root-exec-path-from-shell)
(require 'root-undo-fu)
(require 'root-magit)
(require 'root-diff-hl)
(require 'root-vertico)
(require 'root-consult)
(require 'root-org-mode)
(require 'root-openwith)

;; Language feature related
(require 'root-eglot)
(require 'root-corfu)
(require 'root-prettier)

;; Specific language modules
(require 'root-rjsx)

;; Disable context menu
(context-menu-mode -1)
