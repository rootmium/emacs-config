(use-package vterm
  :config
  ;; Switch windows in insert mode
  (evil-define-key 'insert vterm-mode-map
    (kbd "M-h") 'windmove-left
    (kbd "M-l") 'windmove-right
    (kbd "M-k") 'windmove-up
    (kbd "M-j") 'windmove-down)
  )

(root/global-keymap "C-c t" 'vterm)

(provide 'root-vterm)
