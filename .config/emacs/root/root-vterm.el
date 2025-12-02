(use-package vterm
  :config
  ;; Send next key to vterm
  (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
  (define-key vterm-mode-map (kbd "M-h") 'windmove-left)
  (define-key vterm-mode-map (kbd "M-l") 'windmove-right)
  (define-key vterm-mode-map (kbd "M-k") 'windmove-up)
  (define-key vterm-mode-map (kbd "M-j") 'windmove-down)
  )

;; Keybind for vterm
(root/global-keymap "C-c t" 'vterm)

(provide 'root-vterm)
