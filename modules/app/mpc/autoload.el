;;; app/mpc/autoload.el -*- lexical-binding: t; -*-

(defvar +mpc-repeat nil "Flag to keep track of mpc repeat state")
(defvar +mpc-single nil "Flag to keep track of mpc repeat state")
(defvar +volume-muted nil "Flag to track the muted state")

;; manage services
;;;###autoload
(defun +mpd-start ()
  "Start MPD if it's not already running and check, then update the MPD database."
  (interactive)
  (if (= 0 (call-process "sh" nil nil nil "-c" "pgrep mpd"))
      (message "MPD was already running.")
    (shell-command "mpd")
    (+mpd-update)
    (message "MPD started.")))

;;;###autoload
(defun +mpd-kill ()
  "Kill MPD."
  (interactive)
  (call-process "killall" nil nil nil "mpd")
  (message "MPD killed."))

;;;###autoload
(defun +mpd-update ()
  "Updates the MPD database synchronously."
  (interactive)
  (call-process "mpc" nil nil nil "update")
  (message "MPD database updated."))



;;;###autoload
(defun +mpc-select-song ()
  "Select and play a song from the current simple-mpc playlist."
  (interactive)
  (save-window-excursion
    (if (get-buffer "*simple-mpc-current-playlist*")
        (with-current-buffer (get-buffer "*simple-mpc-current-playlist*")
          (let* ((current-line-number (line-number-at-pos))
                 (lines (cl-loop
                         with min-line-number = (line-number-at-pos (point-min))
                         with buffer-text-lines = (split-string (buffer-string) "\n")
                         with lines = nil
                         for l in buffer-text-lines
                         for n = min-line-number then (1+ n)
                         do (push (cons l n) lines)
                         finally return (nreverse lines)))
                 (selected-line (completing-read "Song: " lines)))
            (when selected-line
              (let ((line (cdr (assoc selected-line lines))))
                (goto-line line)
                (simple-mpc-play-current-line)))))
 ;; If the buffer doesn't exist, create it and proceed
   (progn
     (simple-mpc-view-current-playlist)
     (+mpc-select-song)))))

;;redefined
;;;###autoload
(defun simple-mpc (&optional ignore-auto noconfirm)
  "Start simple-mpc.

IGNORE-AUTO and NOCONFIRM are passed by `revert-buffer'."
  (interactive)
  (let ((buf (get-buffer-create simple-mpc-main-buffer-name)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert (propertize "* simple-mpc *\n\n"
                          'face 'simple-mpc-main-name)
              (propertize "   * controls\n" 'face 'simple-mpc-main-headers)
              "      * [t]oggle\n"
              "      * [n]ext track\n"
              "      * [p]revious track\n"
              "      * seek [h]orward\n"
              "      * seek [a]ackward\n"
              "      * increase [d]olume\n"
              "      * decrease [o]olume\n"
              "      * toggle [r]epeat mode\n"
              (propertize "\n   * playlist\n" 'face 'simple-mpc-main-headers)
              "      * view [c]urrent playlist\n"
              "      * [C]lear current playlist\n"
              "      * [S]huffle playlist\n"
              "      * [l]oad playlist\n"
              "      * [s]earch database\n"
              (propertize "\n   * misc\n" 'face 'simple-mpc-main-headers)
              "      * [q]uit")
      (simple-mpc-mode) ; start major mode
      (switch-to-buffer buf))))

;;misc stuff
;;;###autoload
(defun +evil-select-all ()
  (interactive)
  (evil-goto-first-line)
  (evil-visual-line)
  (evil-goto-line)
  (previous-line))

;;;###autoload
(defun +mpc-shift (n)
  (let* ((myln (line-number-at-pos))
         (targetline (+ myln n)))
    (shell-command (concat "mpc move " (number-to-string myln) " " (number-to-string targetline) ))
    (simple-mpc-view-current-playlist)
    (goto-line targetline)))

;;;###autoload
(defun +mpc-shift-up ()
  (interactive)
  (mpc-shift -1)
  )

;;;###autoload
(defun +mpc-shift-down ()
  (interactive)
  (mpc-shift 1)
  )



;;;###autoload
(defun simple-mpc-toggle-repeat ()
  "Toggle repeat mode."
  (interactive)
  (simple-mpc-call-mpc nil "repeat")
  (setq +mpc-repeat (not +mpc-repeat))
  (message "%s" "Toggled repeat mode."))

;;;###autoload
(defun +mpc-toggle-single-cycle ()
  "Toggle repeat mode."
  (interactive)
  (let ((single (cond
                 ((eq +mpc-single 'on) 'off)
                 ((eq +mpc-single 'off) 'once)
                 (t 'on))))
    (setq +mpc-single single)
    (simple-mpc-call-mpc nil (list "single" (symbol-name single))))
  (message "Toggled single mode: %s" +mpc-single))

;;;###autoload
(defun mpc-current-song ()
  "Return the current playing song in MPC."
  (let* ((status (shell-command-to-string "mpc status"))
         (state (string-match "playing" status))
         (song (shell-command-to-string "mpc current")))
    (if state
        (concat "Playing: " song)
      (concat "Not playing any song."))))


(defun mpd-save-excursion (& forms)
  (let ((myln (line-number-at-pos)))
    (progn forms)
    (simple-mpc-view-current-playlist)
    (goto-line myln)
    ))

;;;###autoload
(defun +mpc-add-everything ()
  (interactive)
  (mpd-save-excursion
   (+evil-select-all)
   (simple-mpc-query-add)))

;;;###autoload
(defun +reload-playlist ()
  (interactive)
  (let (
        (myplaylist (read-string "Name of Playlist: "))
        )
    (shell-command (concat "mpc clear " myplaylist))
    (shell-command (concat "mpc save " myplaylist)))
  )

;;REVIEW
;;;###autoload
(defun +mpd-remove-dupes ()
  (interactive)
  (let ((myln (line-number-at-pos)))
    (shell-command "mpd-remove-dupes.sh")
    (simple-mpc-view-current-playlist)
    (goto-line myln)
    )
  )

;;;###autoload
(defun +delete-mpc-item-without-losing-place ()
  (interactive)
  (let* ((myln (line-number-at-pos))
         (priorline (- myln 1)))
    (simple-mpc-delete)
    (goto-line priorline)))

;;;###autoload
(defun +save-playlist ()
  (interactive)
  (let (
        (myplaylist (read-string "Name of Playlist: "))
        )
    (shell-command (concat "mpc rm " myplaylist))
    (shell-command (concat "mpc save " myplaylist))))
;;TODO ;; use defcuston and let the user cutomize
;; in case you want to change system volume
;;
(setq +sys-vol-min 1)

(defcustom +sys-vol-max 95
  "The maximum system volume."
  :type 'integer
  :group 'system-volume)

(defcustom +sys-vol-step 5
  "The increment step for adjusting system volume."
  :type 'integer
  :group 'system-volume)

(defun +get-volume ()
  (* +sys-vol-step (round (string-to-number
                                ;; (shell-command-to-string "awk -F\"[][]\" '/dB/ { print $2 }' <(amixer sget Master)"))
                                ;; (shell-command-to-string "awk -F\"[][]\" '/dB/ { print $2 }' <(pamixer --get-volume)"))
                                (shell-command-to-string "pamixer --get-volume"))
                               +sys-vol-step)))

;;;###autoload
(defun +volume-toggle-mute ()
  "Toggle the mute state of the volume"
  (interactive)
  (setq +volume-muted (not +volume-muted))
  (save-window-excursion
  (if +volume-muted
      (async-shell-command "pamixer --mute &")
    (async-shell-command "pamixer --unmute &"))))

;;;###autoload
(defun +set-volume (level)
  (interactive "nVolume level: ")
  (let ((clipped-level
         (cond ((< level +sys-vol-min) +sys-vol-min)
               ((> level +sys-vol-max) +sys-vol-max)
               (t level))))
    (save-window-excursion
      (shell-command
       ;; (format "amixer set Master %s%% &" clipped-level) nil nil))))
       (format "pamixer --set-volume %s &" clipped-level) nil nil))))

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
