(defun root/global-keymap (&rest k+c)
  "Set global keymaps"
  (let (key cmd)
    (while k+c
      (keymap-global-set (car k+c) (cadr k+c))
      (setq k+c  (cddr k+c)))))

(provide 'root-custom-functions)
