(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (
         ("C-c F" . consult-find)
         ("C-c r" . consult-recent-file)
         ("C-c s" . consult-line)
         ("C-c /" . consult-ripgrep)
         ("C-x b" . consult-buffer)
         )
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-recent-file
   consult-buffer
   :preview-key "M-.")
  (setq consult-buffer-filter '(
                                "\\` "
                                "\\`\\*Completions\\*\\'"
                                "\\`\\*Flymake log\\*\\'"
                                "\\`\\*Help\\*\\'"
                                "\\`\\*Messages\\*\\'"
                                "\\`\\*Async-native-compile-log\\*\\'"
                                "\\`magit-process.*\\'"
                                "\\`magit-diff.*\\'"
                                "\\`\\*Native-compile-Log\\*\\'"
                                "\\`\\*Backtrace\\*\\'"
                                "\\`\\*Warnings\\*\\'"
                                "\\`\\*Async.*\\'"
                                "\\`\\*EGLOT.*\\'"
                                "\\`\\*eldoc.*\\'"
                                ))
  )

(provide 'root-consult)
