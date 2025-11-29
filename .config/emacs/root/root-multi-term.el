;; Use multi term to manage multiple terminals
(use-package multi-term)

;; Global keybind to start terminals
(root/global-keymap "C-c t" 'multi-term)

(provide 'root-multi-term)
