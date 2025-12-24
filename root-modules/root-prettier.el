(use-package prettier-js)

(dolist (mode '(html-mode-hook
                js-mode-hook
                typescript-ts-mode-hook
                css-mode-hook))
  (add-hook mode (lambda ()
                   (prettier-js-mode))))

(provide 'root-prettier)
