(use-package vterm
  :config
  ;; Send next key to vterm
  (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
  
  (evil-define-key 'insert vterm-mode-map
    ;; Switch windows in insert mode
    (kbd "M-h") 'windmove-left
    (kbd "M-l") 'windmove-right
    (kbd "M-k") 'windmove-up
    (kbd "M-j") 'windmove-down

    ;; Send these keys directly to vterm
    (kbd "C-e") #'vterm--self-insert
    (kbd "C-f") #'vterm--self-insert
    (kbd "C-a") #'vterm--self-insert
    (kbd "C-v") #'vterm--self-insert
    (kbd "C-b") #'vterm--self-insert
    (kbd "C-n") #'vterm--self-insert
    (kbd "C-p") #'vterm--self-insert
    (kbd "C-r") #'vterm--self-insert
    (kbd "C-t") #'vterm--self-insert
    
    ;; Send next key to vterm
    (kbd "C-q") #'vterm-send-next-key
    )
  )

;; Keybind for vterm
(root/global-keymap "C-c t" 'vterm)

(provide 'root-vterm)
