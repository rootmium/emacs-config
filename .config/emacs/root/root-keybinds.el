(root/global-keymap "C-S-c" 'kill-ring-save
		                 "C-S-v" 'yank
                     "M-;" 'comment-line
                     "C-x C-b" 'nil

                     ;; Switch windows with Meta(Alt) + hjkl
                     "M-h" 'windmove-left
                     "M-l" 'windmove-right
                     "M-k" 'windmove-up
                     "M-j" 'windmove-down
                     )

(provide 'root-keybinds)
