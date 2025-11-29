;; -*- lexical-binding: t; -*-

;; Load modules
(add-to-list 'load-path "~/.config/emacs/root")

(require 'root-theme)
(require 'root-package)
(require 'root-base-config)
(require 'root-custom-functions)
(require 'root-keybinds)
(require 'root-dired)

(require 'root-multi-term)
(require 'root-doom-modeline)
(require 'root-exec-path-from-shell)
(require 'root-undo-tree)
(require 'root-evil)
(require 'root-magit)
(require 'root-git-gutter)
(require 'root-vertico)
(require 'root-consult)
(require 'root-org-mode)
(require 'root-which-key)
(require 'root-projectile)
(require 'root-eglot)
