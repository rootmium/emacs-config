(root/global-keymap "C-S-c" 'kill-ring-save
		                "C-S-v" 'yank
                    "M-;" 'comment-line
                    "C-x C-b" 'nil
                    "C-c t" '(lambda () (interactive) (ansi-term "/bin/zsh"))
                    "C-c m m" 'root/mpv-menu
                    "M-o" 'pop-to-mark-command
                    "C-c b" 'ibuffer
                    "C-c e d" 'root/duplicate-line-or-region
                    "C-c e f" 'root/indent-clean-buffer
                    "C-c e e" 'root/kill-current-line
                    )

(provide 'root-keybinds)
