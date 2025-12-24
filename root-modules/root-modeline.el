;; Right side of the modeline
(defvar root/modeline-right
  (list
   ;; Git branch
   '(:eval (when (and (boundp 'vc-mode) vc-mode)
             (propertize (substring vc-mode 5)
                         'face 'font-lock-comment-face)))
   " "
   
   ;; Flymake diagnostics
   '(:eval (when (bound-and-true-p flymake-mode)
             flymake-mode-line-format))
   " "
   
   ;; Major mode
   '(:propertize mode-name face font-lock-string-face)
   " "))

;; Left side of the modeline
(defvar root/modeline-left
  (list
   "%e"
   mode-line-front-space
   ;; File status
   '(:eval (cond (buffer-read-only (propertize " RO " 'face 'error))
                 ((buffer-modified-p) (propertize " MO " 'face 'warning))
                 (t (propertize " OK " 'face 'success))))
   ;; File name
   '(:eval (propertize " %b " 'face 'bold))
   
   ;; Position
   '(:eval (propertize " %p " 'face 'success))
   
   ;; Line and column
   '(:eval (propertize " [%l:%c] " 'face 'warning))))

;; Set modeline
(setq-default mode-line-format
  (list
   root/modeline-left
   '(:eval (let* ((r-str (format-mode-line root/modeline-right))
                  (r-w (string-width r-str)))
             ;; Align to right margin minus the width of the right content
             (propertize " " 'display `((space :align-to (- (+ right right-margin) ,r-w))))))
   root/modeline-right))

(provide 'root-modeline)
