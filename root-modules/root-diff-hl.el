(use-package diff-hl
  :hook
  ((prog-mode text-mode) . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-pre-refresh . diff-hl-magit-pre-refresh)
  (magit-post-refresh . diff-hl-magit-post-refresh))

(provide 'root-diff-hl)
