(use-package ob-http)

(use-package org
  :ensure nil
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (http . t)))
  )

(provide 'root-org-mode)
