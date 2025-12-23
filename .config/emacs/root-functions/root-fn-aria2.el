(require 'ansi-color)
(require 'notifications)

(defun root/aria2-process-filter (process output)
  (let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((pmark (process-mark process)))
          (save-excursion
            (goto-char pmark)
            (when (string-match "\r" output)
              (delete-region (line-beginning-position) (point))
              (setq output (replace-regexp-in-string "\r" " " output)))
            (insert (ansi-color-apply output))
            (set-marker pmark (point))))))))

(defun root/aria2-sentinel (process event)
  (when (memq (process-status process) '(exit signal))
    (let ((status (if (string-match-p "finished" event)
                      "Download Complete!"
                    "Download failed/stopped")))
      (notifications-notify
       :title "Emacs aria2"
       :body (format "%s\nProcess %s" status event)
       :urgency 'normal
       :app-name "Emacs"))
    (message "Aria2 process: %s" (string-trim event))
    (with-current-buffer (process-buffer process)
      (goto-char (point-max))
      (insert (format "\n--- Process %s ---\n" (string-trim event))))))

(defun root/aria2--run-process (buffer args)
  (with-current-buffer buffer
    (setq-local window-point-insertion-type t)
    (display-buffer buffer))
  (let ((proc (apply 'start-process "aria2-process" buffer "aria2c" args)))
    (set-process-filter proc 'root/aria2-process-filter)
    (set-process-sentinel proc 'root/aria2-sentinel)
    (message "Aria2 Download Started...")))

(defun root/aria2 (url dest-dir file-name)
  (interactive
   (list (read-string "URL: ")
         (read-directory-name "Download directory: " "~/Downloads/")
         (read-string "File name (empty for default): ")))
  (let* ((buffer-name "*aria2 downloader*")
         (buffer (get-buffer-create buffer-name))
         (args (list "--dir" (expand-file-name dest-dir))))
    (unless (string-empty-p file-name)
      (setq args (append args (list "--out" file-name))))
    (setq args (append args (list url)))
    (root/aria2--run-process buffer args)))

(defun root/aria2-batch (input-file dest-dir)
  (interactive
   (list (read-file-name "URL file: ")
         (read-directory-name "Download directory: " "~/Downloads/")))
  (let* ((buffer-name "*aria2 downloader*")
         (buffer (get-buffer-create buffer-name))
         (args (list "--dir" (expand-file-name dest-dir)
                     "--input-file" (expand-file-name input-file))))
    (root/aria2--run-process buffer args)))

(provide 'root-fn-aria2)
