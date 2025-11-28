(root/global-keymap "C-S-c" 'kill-ring-save
		                 "C-S-v" 'yank
                     "M-;" 'comment-line
                     "C-c t" #'(lambda () (interactive) (term "/bin/zsh")))

(provide 'root-keybinds)
