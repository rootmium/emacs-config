(use-package doom-modeline
  :init
  (setq doom-modeline-icon nil
        doom-modeline-height 0
        doom-modeline-bar-width 0
        doom-modeline-project-detection 'file-name
        )
  :config
  (doom-modeline-mode 1))

(provide 'root-doom-modeline)
