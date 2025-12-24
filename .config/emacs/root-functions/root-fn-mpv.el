(require 'comint)
(defcustom root/mpv-config-path "~/.config/mpv/mpv.conf"
  "Path to the mpv configuration file."
  :type 'file
  :group 'root/custom)

(defcustom root/music-directory "~/Others/Music/"
  "Default directory to look for music"
  :type 'directory
  :group 'root/custom)

(defun root/get-mpv-volume ()
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

(defun root/set-mpv-volume (volume)
  "Set the mpv VOLUME in both the config file and the running player.

VOLUME should be an integer (e.g., 50 for 50%)

This function performs two actions:
1. Updates or appends the 'volume=' key in `root/mpv-config-path'.
2. Uses 'playerctl' to set volume of the running mpv instance."
  (interactive (list (read-number "Set mpv volume: " (root/get-mpv-volume))))
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
    (message "Mpv volume set to %d (config and playerctl)" volume)))

(defun root/mpv-shuffle-dir (dir)
  "Launch mpv in a comint buffer to play files in DIR in shuffle mode."
  (interactive "DSelect directory to play: ")
  (let ((dir-path (expand-file-name dir)))
    (make-comint-in-buffer "mpv-shuffle" "*mpv-output*" "mpv" nil "--shuffle" dir-path)
    (message "Starting mpv shuffle in %s..." dir)))

(defun root/get-current-media-title ()
  "Fetch the currently playing media title using playerctl.

Returns a message if no player is active."
  (interactive)
  (let ((title (shell-command-to-string "playerctl metadata --format '{{title}}' 2>/dev/null")))
    (if (string-empty-p (string-trim title))
        (message "No media playing")
      (message "Now playing: %s" (string-trim title)))))

(defun root/stop-media-player ()
  "Stop the current media player using playerctl."
  (interactive)
  (let ((output (shell-command-to-string "playerctl stop")))
    (if (string-empty-p (string-trim output))
        (message "Player stopped")
      (message "No media playing"))))
