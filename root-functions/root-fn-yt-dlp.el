(defvar root/yt-dlp--process-name "yt-dlp"
  "Internal name used for yt-dlp process.")

(defvar root/yt-dlp--buffer-name " *yt-dlp-output*"
  "Internal name used for yt-dlp buffer.")

(defvar root/yt-dlp--mp3-common-args
  (list "-x"
        "--audio-format" "mp3"
        "--metadata-from-title" "%(title)s"
        "--parse-metadata" "channel:%(artist)s"
        "--parse-metadata" ":(?P<meta_comment>)"
        "--parse-metadata" ":(?P<meta_description>)"
        "--parse-metadata" ":(?P<meta_date>)"
        "--embed-metadata"
        "--embed-thumbnail"
        "--ppa" "EmbedThumbnail+ffmpeg_o:-c:v mjpeg -vf crop=\"'if(gt(ih,iw),iw,ih)':'if(gt(iw,ih),ih,iw)'\""
        "-o" "%(title)s.%(ext)s")
  "Common arguments for yt-dlp mp3 conversion.")

(defun root/yt-dlp-mp3 (url dest-dir)
  "Download a single audio from URL as an MP3.

This function downloads audio (mp3) and applies metadata as specified in
`root/yt-dlp--mp3-common-args'. It saves the file in DEST-DIR."
  (interactive
   (list (read-string "URL: ")
         (read-directory-name "Download directory: " root/music-directory)))
  (let ((args (append root/yt-dlp--mp3-common-args (list "-P" (expand-file-name dest-dir) url))))
    ;; apply is needed for complex args
    (apply #'make-comint-in-buffer root/yt-dlp--process-name root/yt-dlp--buffer-name "yt-dlp" nil args)
    (display-buffer root/yt-dlp--buffer-name)
    (message "Download Started...")))

(defun root/yt-dlp-mp3-batch (input-file dest-dir)
    "Download multiple audio from INPUT-FILE as an MP3.

This function downloads audio (mp3) and applies metadata as specified in
`root/yt-dlp--mp3-common-args'. It saves the file in DEST-DIR. INPUT-FILE
must have video URLs seperated by space or new line."
  (interactive
   (list (read-file-name "URL file: ")
         (read-directory-name "Download directory: " root/music-directory)))
  (let ((args (append root/yt-dlp--mp3-common-args (list "-P" (expand-file-name dest-dir) "-a" (expand-file-name input-file)))))
    (apply #'make-comint-in-buffer root/yt-dlp--process-name root/yt-dlp--buffer-name "yt-dlp" nil args)
    (display-buffer root/yt-dlp--buffer-name)
    (message "Download Started...")))

(provide 'root-fn-yt-dlp)
