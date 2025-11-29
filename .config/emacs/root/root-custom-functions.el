(defun root/global-keymap (&rest k+c)
  "Set global keymaps"
  (let (key cmd)
    (while k+c
      (keymap-global-set (car k+c) (cadr k+c))
      (setq k+c  (cddr k+c)))))

;; Custom function to paste in the terminal buffer
(defun root/term-yank ()
  "Switch to `term-line-mode' paste the text
then switch back to `term-char-mode'"
  (interactive)
  (term-line-mode)
  (yank)
  (term-char-mode)
  )

(provide 'root-custom-functions)
