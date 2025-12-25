(root/global-keymap "C-S-c" 'kill-ring-save
		                 "C-S-v" 'yank
                     "M-;" 'comment-line
                     "C-x C-b" 'nil
                     "C-c t" '(lambda () (interactive) (ansi-term "/bin/zsh"))
                     "C-c m m" 'root/mpv-menu
                     )
                     "C-c e d" 'root/duplicate-line-or-region

(provide 'root-keybinds)
