(require 'comint)
(require 'transient)

(defcustom root/mpv-config-path "~/.config/mpv/mpv.conf"
  "Path to the mpv configuration file."
  :type 'file
  :group 'root/custom)

(defcustom root/music-directory "~/Others/Music/"
  "Default directory to look for music"
  :type 'directory
  :group 'root/custom)

(defvar root/mpv--process-name "mpv-player"
  "Internal name used for mpv process.")

(defvar root/mpv--buffer-name " *mpv-output*"
  "Internal name used for mpv buffer.")

(defun root/mpv-get-volume ()
  "Return the current mpv volume as in integer.

The volume is determined in the following order of priority:
1. Querying 'playerctl' for the active player volume.
2. Parsing the 'volume=' setting from the file at `root/mpv-config-path'
3. Returning a default value of 100 if neither method succeeds."
  (let ((player-vol (shell-command-to-string "playerctl volume 2>/dev/null"))
        (conf-path (expand-file-name root/mpv-config-path)))
    (cond
     ((not (string-empty-p player-vol))
      (round (* 100 (string-to-number player-vol))))
     ((file-exists-p conf-path)
      (with-temp-buffer
        (insert-file-contents conf-path)
        (if (re-search-forward "^volume=\\([0-9]+\\)" nil t)
            (string-to-number (match-string 1))
          100)))
     (t 100))))

(defun root/mpv-set-volume (volume)
  "Set the mpv VOLUME in both the config file and the running player.

VOLUME should be an integer (e.g., 50 for 50%)

This function performs two actions:
1. Updates or appends the 'volume=' key in `root/mpv-config-path'.
2. Uses 'playerctl' to set volume of the running mpv instance."
  (interactive (list (read-number "Set mpv volume: " (root/mpv-get-volume))))
  (let* ((conf-file (expand-file-name root/mpv-config-path))
        (vol (format "volume=%d" volume))
        (float-vol (number-to-string (/ volume 100.0))))
    
    (with-temp-file conf-file
      (when (file-exists-p conf-file)
        (insert-file-contents conf-file))
      (goto-char (point-min))
      (if (re-search-forward "^volume=[0-9]+" nil t)
          (replace-match vol)
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert vol "\n")))

    (shell-command (concat "playerctl --player=mpv volume " float-vol))
    (message "Mpv volume set to %d (config and playerctl)." volume)))

(defun root/mpv-shuffle-dir (dir)
  "Launch mpv in a comint buffer to play files in DIR in shuffle mode.

This function uses `root/music-directory' as base path. Any existing mpv
process is killed with `root/mpv-stop' before starting the new one."
  (interactive
   (list (read-directory-name "Select directory to play: " root/music-directory)))
  (let ((dir-path (expand-file-name dir)))
    (root/mpv-stop)
    (make-comint-in-buffer root/mpv--process-name "*mpv-output*" "mpv" nil "--shuffle" dir-path)
    (message "Starting mpv shuffle in %s..." dir)))

(defun root/mpv-play-file ()
  "Prompt for a file in `root/music-directory' and play it using mpv.

Recursively searches for files in the selected subdirectory. If a file
is selected, any existing mpv process is killed with `root/mpv-stop'
before starting the new one."
  (interactive)
  (let* ((target-dir (read-directory-name "Select directory: " root/music-directory))
         (files (directory-files-recursively target-dir "" nil))
         (selected-file (completing-read "Select file to play: " files nil t)))
    (if (and selected-file (file-exists-p selected-file))
        (progn
          (root/mpv-stop)
          (message "Playing: %s" (file-name-nondirectory selected-file))
          (start-process root/mpv--process-name nil "mpv" (expand-file-name selected-file)))
      (message "No file selected."))))

(defun root/playerctl-title ()
  "Fetch the currently playing media title using playerctl.

Returns a message if no player is active."
  (interactive)
  (let ((title (shell-command-to-string "playerctl metadata --format '{{title}}' 2>/dev/null")))
    (if (string-empty-p (string-trim title))
        (message "No media playing.")
      (message "Now playing: %s" (string-trim title)))))

(defun root/playerctl-stop ()
  "Stop the current media player using playerctl."
  (interactive)
  (let ((output (shell-command-to-string "playerctl stop")))
    (if (string-empty-p (string-trim output))
        (message "Player stopped.")
      (message "No media playing."))))

(defun root/playerctl-play-pause ()
  "Play or pause the current media player using playerctl."
  (interactive)
  (let ((output (shell-command-to-string "playerctl play-pause")))
    (if (string-empty-p (string-trim output))
        (message "Player paused.")
      (message "No media playing."))))

(defun root/playerctl-mpv-command (args)
  "Helper to run playerctl commands for mpv with ARGS."
  (let ((cmd (concat "playerctl --player=mpv " args)))
    (shell-command-to-string cmd)))

(defun root/mpv-stop ()
  "Stop the running mpv process and clear any active playback.

This function first attempts to finds and delete an internal Emacs
process named `root/mpv--process-name'. If no internal process is
found, it attempts to stop an external instance via
`root/playerctl-mpv-command'."
  (interactive)
  (cond ((get-process root/mpv--process-name)
         (delete-process root/mpv--process-name)
         (message "Mpv stopped."))
        ((string-empty-p (string-trim (root/playerctl-mpv-command "stop")))
         (message "Mpv stopped."))
    (t (message "No mpv process is running."))))

(defun root/mpv-play-pause ()
  "Play or pause mpv using playerctl."
  (interactive)
  (let ((output (root/playerctl-mpv-command "play-pause")))
    (if (string-empty-p (string-trim output))
        (message "Mpv paused.")
      (message "No media playing."))))

(defun root/mpv-next ()
  "Play next media in the playlist using playerctl."
  (interactive)
  (let ((output (root/playerctl-mpv-command "next")))
    (if (string-empty-p (string-trim output))
        (message (root/playerctl-title))
      (message "No media playing."))))

(defun root/mpv-previous ()
  "Play previous media in the playlist using playerctl."
  (interactive)
  (let ((output (root/playerctl-mpv-command "previous")))
    (if (string-empty-p (string-trim output))
        (message (root/playerctl-title))
      (message "No media playing."))))

(defun root/mpv-seek-forward ()
  "Seek forward 10 seconds using playerctl."
  (interactive)
  (let ((output (root/playerctl-mpv-command "position 10+")))
    (if (string-empty-p (string-trim output))
        (message "Forward 10 seconds")
      (message "No media playing."))))

(defun root/mpv-seek-rewind ()
  "Rewind 10 seconds using playerctl."
  (interactive)
  (let ((output (root/playerctl-mpv-command "position 10-")))
    (if (string-empty-p (string-trim output))
        (message "Rewind 10 seconds")
      (message "No media playing."))))

(transient-define-prefix root/mpv-menu ()
  "Transient menu for controlling mpv via playerctl"
  [:description (lambda () (root/playerctl-title))
  ["Open"
   ("o" "Open" root/mpv-play-file)
   ("z" "Shuffle" root/mpv-shuffle-dir)
   ]
  ["Navigation"
   ("SPC" "Play/Pause" root/mpv-play-pause)
   ("s" "Stop" root/mpv-stop)
   ("n" "Next" root/mpv-next)
   ("p" "Previous" root/mpv-previous)
   ]
  ["Volume/Seek"
   ("v" "Volume" root/mpv-set-volume)
   ("f" "Forward" root/mpv-seek-forward)
   ("r" "Rewind" root/mpv-seek-rewind)
   ]
  ["Others"
   ("u" "Play/Pause" root/playerctl-play-pause)
   ("k" "Stop" root/playerctl-stop)
   ]])

(provide 'root-fn-media-playback)
