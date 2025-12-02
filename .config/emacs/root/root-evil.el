(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-auto-balance-windows nil
        evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (setq-default evil-shift-width tab-width)

  (evil-set-initial-state 'git-commit-mode 'insert)
  (evil-set-initial-state 'vterm-mode 'insert)
  (evil-set-initial-state 'dired-mode 'emacs)

  (define-key evil-normal-state-map (kbd "0") 'evil-first-non-blank)
  (define-key evil-motion-state-map (kbd "0") 'evil-first-non-blank)
  (define-key evil-visual-state-map (kbd "0") 'evil-first-non-blank)

  (evil-define-key 'normal org-mode-map (kbd "M-h") 'windmove-left)
  (evil-define-key 'normal org-mode-map (kbd "M-j") 'windmove-down)
  (evil-define-key 'normal org-mode-map (kbd "M-k") 'windmove-up)
  (evil-define-key 'normal org-mode-map (kbd "M-l") 'windmove-right)

  (evil-define-key 'insert org-mode-map (kbd "M-h") 'windmove-left)
  (evil-define-key 'insert org-mode-map (kbd "M-j") 'windmove-down)
  (evil-define-key 'insert org-mode-map (kbd "M-k") 'windmove-up)
  (evil-define-key 'insert org-mode-map (kbd "M-l") 'windmove-right)
  )

(provide 'root-evil)
