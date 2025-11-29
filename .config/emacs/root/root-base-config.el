;; Only ask y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Core settings
(setq inhibit-startup-screen t
      inhibit-startup-message t
      initial-scratch-message ";; Emacs here!"
      auto-save-default nil
      make-backup-files nil
      large-file-warning-threshold nil
      vc-follow-symlinks t
      global-auto-revert-non-file-buffers t
      native-comp-async-report-warnings-errors nil
      help-window-select t
      recentf-max-saved-items 2000
      undo-limit 100000000
      scroll-conservatively 101
      scroll-step 1
      scroll-margin 5
      custom-safe-themes t
      )

;; Fix scroll margin for term-mode
(add-hook 'term-mode-hook #'(lambda () (setq-local scroll-margin 0)))

;; Modes
(savehist-mode 1)
(column-number-mode 1)
(global-auto-revert-mode 1)
(recentf-mode 1)
(save-place-mode 1)

;; Special modes for programming and writing
(dolist (mode '(text-mode-hook
                prog-mode-hook
                org-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda ()
		   (display-line-numbers-mode 1)
                   (hl-line-mode 1)
                   (display-fill-column-indicator-mode 1)
                   (electric-pair-mode 1)
                   (show-paren-mode)
                   )))

;; Show vertical column
(setq-default display-fill-column-indicator-column 79)

;; Tabs to spaces
(setq-default indent-tabs-mode nil
	            tab-width 2)

;; Periodicly save recent files
(run-with-idle-timer 600 t (lambda ()
                             (let ((save-silently t))
                               (recentf-save-list))))

;; Save custom variables somewhere else
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror 'nomessage)

;; Start typescript mode in .ts files
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(provide 'root-base-config)
