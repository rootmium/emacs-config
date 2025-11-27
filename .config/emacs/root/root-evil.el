(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :init
  (evil-collection-init))

(provide 'root-evil)
