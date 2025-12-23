(defun root/global-keymap (&rest k+c)
  "Set global keymaps"
  (let (key cmd)
    (while k+c
      (keymap-global-set (car k+c) (cadr k+c))
      (setq k+c  (cddr k+c)))))

;; Custom function to paste in the terminal buffer
(defun root/term-yank ()
  "Switch to `term-line-mode' paste the text
then switch back to `term-char-mode'"
  (interactive)
  (term-line-mode)
  (yank)
  (term-char-mode)
  )

;; Upload selected file to 0x0
(defun root/upload-to-0x0 ()
  "Upload file to 0x0.st
The URL is saved to the clipboard upon completion"
  (interactive)
  (let* ((file (expand-file-name (read-file-name "File to upload: ")))
         (file-name (file-name-nondirectory file))
         (process-buffer (generate-new-buffer (format " *0x0-upload-%s*" file-name))))
    (message "Uploading %s..." file-name)

    (make-process
     :name "0x0-upload"
     :buffer process-buffer
     :command (list "curl" "-sF" (format "file=@%s" file) "-Fexpires=24" "https://0x0.st")
     :sentinel
     (lambda (proc event)
       (when (string-equal event "finished\n")
         (let ((url (with-current-buffer (process-get proc :output-buffer)
                      (string-trim (buffer-string)))))
           (if (string-match-p "https://0x0\\.st/" url)
               (progn
                 (kill-new url)
                 (message "Upload finished! (24H) URL copied: %s" url))
             (message "Upload failed: %s" url))
           (kill-buffer (process-get proc :output-buffer))))))
    (process-put (get-process "0x0-upload") :output-buffer process-buffer)
    nil))

(provide 'root-fn-miscellaneous)
