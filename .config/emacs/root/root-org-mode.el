(use-package ob-http)

(use-package org
  :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (http . t)))
  (define-key org-mode-map (kbd "M-h") 'windmove-left)
  (define-key org-mode-map (kbd "M-l") 'windmove-right)
  (define-key org-mode-map (kbd "M-k") 'windmove-up)
  (define-key org-mode-map (kbd "M-j") 'windmove-down)
  )

(provide 'root-org-mode)
