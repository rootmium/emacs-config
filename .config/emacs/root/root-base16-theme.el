(use-package base16-theme
  :ensure t
  :config
  (setq base16-theme-distinct-fringe-background nil
        base16-theme-highlight-mode-line t)
  (load-theme 'base16-catppuccin-mocha t)
  (custom-set-faces
   `(font-lock-comment-face ((t (:slant italic))))
   `(font-lock-variable-name-face ((t (:slant italic))))
   )
  )

(provide 'root-base16-theme)
