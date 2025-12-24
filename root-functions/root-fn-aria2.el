(require 'comint)
(require 'notifications)

(defun root/aria2-sentinel (process event)
  "Sentinel for aria2 PROCESS to handle notifications on EVENT.

Triggers a desktop notification via `notifications-notify' when the
PROCESS finishes or fails, and shows a `message' in the minibuffer"
  (when (memq (process-status process) '(exit signal))
    (let ((status (if (string-match-p "finished" event)
                      "Download Complete!"
                    "Download failed/stopped")))
      (notifications-notify
       :title "Emacs aria2"
       :body (format "%s\nProcess %s" status event)
       :urgency 'normal
       :app-name "Emacs"))
    (message "Aria2 process: %s" (string-trim event))))


(defun root/aria2--run-process (buffer args)
  "Internal helper to start the aria2 process.

Starts 'aria2c' using ARGS, outputting to BUFFER. If BUFFER does not
exist, it is created and set to `comint-mode`. Sets up the sentinel
for notifications."
  (let ((buf (get-buffer-create buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'comint-mode)
        (comint-mode)
        (setq-local comint-inhibit-carriage-motion nil))
      (let ((proc (get-buffer-process buf)))
        (if (and proc (process-live-p proc))
            (error "An aria2 process is already running in %s" buffer-name)
          (apply 'make-comint-in-buffer "aria2-process" buf "aria2c" nil args)
          (set-process-sentinel (get-buffer-process buf) 'root/aria2-sentinel)
          (display-buffer buf)
          (message "Aria2 download started...")))))
)

(defun root/aria2 (url dest-dir file-name)
  "Download URL to DEST-DIR as FILE-NAME using aria2.

If FILE-NAME is empty, aria2 will determine the filename from the URL."
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
  "Download multiple URLs from the INPUT-FILE to DEST-DIR using aria2.

INPUT-FILE should be a text file containing one URL per line."
  (interactive
   (list (read-file-name "URL file: ")
         (read-directory-name "Download directory: " "~/Downloads/")))
  (let* ((buffer-name "*aria2 downloader*")
         (buffer (get-buffer-create buffer-name))
         (args (list "--dir" (expand-file-name dest-dir)
                     "--input-file" (expand-file-name input-file))))
    (root/aria2--run-process buffer args)))

(provide 'root-fn-aria2)
