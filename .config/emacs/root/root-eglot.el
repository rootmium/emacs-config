(use-package eldoc-box)
(use-package eglot
  :ensure nil
  :config
  (setq eglot-events-buffer-size 0
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        read-process-output-max (* 1024 1024))
  )

(setq js-indent-level 2
      css-indent-offset 2)

;; Mode setup
(dolist (mode '(c-mode-hook
                c++-mode-hook
                html-mode-hook
                js-mode-hook
                typescript-ts-mode-hook
                python-mode-hook
                css-mode-hook))
  (add-hook mode (lambda ()
                   (eglot-ensure))))

;; keybinds
(root/global-keymap "C-c K" 'eldoc-box-help-at-point
                     "C-c l a" 'eglot-code-actions
                     "C-c l c" 'eglot-rename
                     "C-c l d" 'xref-find-definitions
                     "C-c l =" 'eglot-format-buffer
                     "C-c l r" 'xref-find-references
                     "C-c l e" 'consult-flymake
                     "C-c l n" 'flymake-goto-next-error
                     "C-c l p" 'flymake-goto-prev-error
                     "C-c l E" 'flymake-show-buffer-diagnostics
                     )

(provide 'root-eglot)
