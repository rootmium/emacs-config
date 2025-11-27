(require 'dired-x)

(setq dired-listing-switches "-AghoX --group-directories-first"
      dired-kill-when-opening-new-dired-buffer t
      dired-hide-details-hide-symlink-targets nil)

(add-hook 'dired-mode-hook
	        (lambda ()
	          (interactive)
	          (dired-hide-details-mode 1)
	          (hl-line-mode 1)))

(provide 'root-dired)
