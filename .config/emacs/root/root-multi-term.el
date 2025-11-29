;; Use multi term to manage multiple terminals
(use-package multi-term)

;; Global keybind to start terminals
(root/global-keymap "C-c t" 'multi-term)

;; Paste in the terminal buffer
(add-to-list 'term-bind-key-alist '("C-S-v" . root/term-yank))

(provide 'root-multi-term)
