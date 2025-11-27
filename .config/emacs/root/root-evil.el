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
  (define-key evil-normal-state-map (kbd "0") 'evil-first-non-blank)
  (define-key evil-motion-state-map (kbd "0") 'evil-first-non-blank)
  (define-key evil-visual-state-map (kbd "0") 'evil-first-non-blank)
  )

(use-package evil-collection
  :config
  (setq evil-collection-setup-minibuffer t)
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-up-directory
    "l" 'dired-find-file)
  :init
  (evil-collection-init))

(provide 'root-evil)
