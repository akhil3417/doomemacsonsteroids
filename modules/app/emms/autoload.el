;;; app/emms/autoload.el -*- lexical-binding: t; -*-

(defvar +volume-muted nil "Flag to track the muted state")

;;;###autoload
(defun +emms/mpd-start-music-daemon ()
  (interactive)
  (start-process "mpd" nil "mpd")
  (+emms/mpc-update-database)
  (emms-player-mpd-connect)
  (emms-cache-set-from-mpd-all)
  (message "MPD Started!"))

;;;###autoload
(defun +emms/mpd-kill-music-daemon ()
  (interactive)
  (emms-stop)
  (call-process "mpd" nil nil nil "--kill")
  (message "MPD Killed!"))

;;;###autoload
(defun +emms/mpc-update-database ()
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "MPD Database Updated!"))

;;;###autoload
(defun +emms/mpd-restart-music-daemon ()
  (interactive)
  (+emms/mpd-kill-music-daemon)
  (+emms/mpd-start-music-daemon)
  (message "MPD Restarted!"))


  (defun +emms-toggle-time-display ()
    "Toggle the display of time information in the modeline"
    (interactive)
    (if emms-playing-time-display-p
        (emms-playing-time-disable-display)
      (emms-playing-time-display-mode)))

  (defun +emms-select-song ()
    "Select and play a song from the current EMMS playlist."
    (interactive)
    (with-current-emms-playlist
      (emms-playlist-mode-center-current)
      (let* ((current-line-number (line-number-at-pos))
             (lines (cl-loop
                     with min-line-number = (line-number-at-pos (point-min))
                     with buffer-text-lines = (split-string (buffer-string) "\n")
                     with lines = nil
                     for l in buffer-text-lines
                     for n = min-line-number then (1+ n)
                     do (push (cons l n)
                              lines)
                     finally return (nreverse lines)))
             (selected-line (completing-read "Song: " lines)))
        (when selected-line
          (let ((line (cdr (assoc selected-line lines))))
            (goto-line line)
            (emms-playlist-mode-play-smart)
            (emms-playlist-mode-center-current))))))

  (defun track-title-from-file-name (file)
    "For using with EMMS description functions. Extracts the track
title from the file name FILE, which just means a) taking only
the file component at the end of the path, and b) removing any
file extension."
    (with-temp-buffer
      (save-excursion (insert (file-name-nondirectory (directory-file-name file))))
      (ignore-error 'search-failed
        (search-forward-regexp (rx "." (+ alnum) eol))
        (delete-region (match-beginning 0) (match-end 0)))
      (buffer-string)))

  (defun my-emms-track-description (track)
    "Return a description of TRACK, for EMMS, but try to cut just
the track name from the file name, and just use the file name too
rather than the whole path."
    (let ((artist (emms-track-get track 'info-artist))
          (title (emms-track-get track 'info-title)))
      (cond ((and artist title)
             ;; Converting the artist/title to a string works around a bug in `emms-info-exiftool'
             ;; where, if your track name is a number, e.g. "1999" by Jeroen Tel, then it will be an
             ;; integer type here, confusing everything.
             ;;
             ;; I would fix the bug properly and submit a patch but I just cannot be bothered to
             ;; figure out how to do that.
             (concat (format "%s" artist) " - " (format "%s" title)))
            (title title)
            ((eq (emms-track-type track) 'file)
             (track-title-from-file-name (emms-track-name track)))
            (t (emms-track-simple-description track)))))

  (setq emms-track-description-function 'my-emms-track-description)


;;;###autoload
(defun +emms-add-to-favorites ()
  "Add the current song to the hard-coded favorites playlist."
  (interactive)
  (save-window-excursion
        (progn
          (with-current-buffer "*Music*"
            (emms-playlist-mode-add-contents))
          (+emms-playlist-save 'm3u "~/Music/fav.m3u")
          (setq emms-playlist-buffer
                (emms-playlist-set-playlist-buffer
                 (get-buffer "*Music*"))))))


;;;###autoload
(defun +emms-playlist-save (format file)
  "Store the current playlist to FILE as the type FORMAT.
The default format is specified by `emms-source-playlist-default-format'."
  (interactive (list (emms-source-playlist-read-format)
                     (read-file-name "Store as: "
                                     emms-source-file-default-directory
                                     emms-source-file-default-directory
                                     nil)))
  (with-temp-buffer
    (emms-source-playlist-unparse format
                                  (with-current-emms-playlist
                                    (current-buffer))
                                  (current-buffer))
    (let ((append-to-file t))
      (write-region (point-min) (point-max) file t))))

(defun my/get-volume ()
  (* my/volume-step (round (string-to-number
                                ;; (shell-command-to-string "awk -F\"[][]\" '/dB/ { print $2 }' <(amixer sget Master)"))
                                ;; (shell-command-to-string "awk -F\"[][]\" '/dB/ { print $2 }' <(pamixer --get-volume)"))
                                (shell-command-to-string "pamixer --get-volume"))
                               my/volume-step)))

(defun my/set-volume (level)
  (interactive "nVolume level: ")
  (let ((clipped-level
         (cond ((< level +sys-vol-min) +sys-vol-min)
               ((> level +sys-vol-max) +sys-vol-max)
               (t level))))
    (save-window-excursion
      (shell-command
       ;; (format "amixer set Master %s%% &" clipped-level) nil nil))))
       (format "pamixer --set-volume %s &" clipped-level) nil nil))))


(defun +volume-toggle-mute ()
  "Toggle the mute state of the volume"
  (interactive)
  (setq +volume-muted (not +volume-muted))
  (save-window-excursion
  (if +volume-muted
      (async-shell-command "pamixer --mute &")
    (async-shell-command "pamixer --unmute &"))))

;;;###autoload
(defun +volume-step-change (delta)
  (+set-volume (+ delta (+get-volume))))

;;;###autoload
(defun +volume-increase ()
  (interactive)
  (+volume-step-change +sys-vol-step))

;;;###autoload
(defun +volume-decrease ()
  (interactive)
  (+volume-step-change (- +sys-vol-step)))

;; ;;in case you are'nt using mpd server you can use this hack to get notifications
;; ; choose D-Bus to disseminate messages, if it is running.
;; (cond
;;  ;; test to see if D-Bus notifications are available
;;  ((if (and (require 'dbus nil t)
;; 	   (dbus-ping :session "org.freedesktop.Notifications"))
;;       (progn
;; 	(setq notify-method 'notify-via-dbus-notifications)
;; 	(require 'notifications))))
;;  ;; could use the message system otherwise
;;  (t (setq notify-method 'notify-via-message)))

;; (defun notify-via-notifications (title msg icon)
;;   "Send notification with TITLE, MSG via `D-Bus'."
;;   (notifications-notify
;;    :title title
;;    :body msg
;;    :app-icon icon
;;    :urgency 'low))

;; (defun notify-via-messages (title msg)
;;   "Send notification with TITLE, MSG to message."
;;   (message "APPOINTMENT: %s" msg))

;; (defun emms-notifications-dbus (track-name)
;;   "Share track name via `D-Bus'."
;;   (let ((icon "/usr/share/icons/gnome/24x24/categories/applications-multimedia.png"))
;;     (notify-via-notifications "EMMS is now playing:" track-name icon)))

;; (defun emms-notifications-message (track-name)
;;   "Share track name via Emacs minibuffer."
;;   (message "EMMS is now playing: %s" track-name))
