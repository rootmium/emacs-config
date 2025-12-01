(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/Projects/git-repos/github/" "~/Projects/git-repos/github/backend-dev-log"))
  :config
  ;; On Linux, however, I usually go with another one
  (define-key projectile-mode-map (kbd "C-c C-p") 'projectile-command-map)
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(provide 'root-projectile)
