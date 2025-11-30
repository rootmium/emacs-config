(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-auto-balance-windows nil
        evil-want-minibuffer t
        evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (setq-default evil-shift-width tab-width)

  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'vterm-mode 'insert)

  (define-key evil-normal-state-map (kbd "0") 'evil-first-non-blank)
  (define-key evil-motion-state-map (kbd "0") 'evil-first-non-blank)
  (define-key evil-visual-state-map (kbd "0") 'evil-first-non-blank)

  (evil-define-key 'normal dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file)
  )

(provide 'root-evil)
