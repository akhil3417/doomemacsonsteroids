;;; app/emms/config.el -*- lexical-binding: t; -*-

(use-package! emms
  ;; :defer t
  ;; :commands (emms)
  :init
  (setq emms-directory (concat doom-data-dir "emms")
        emms-cache-file (concat doom-cache-dir "emms"))
  :config
  (emms-all)
  (emms-default-players)
  ;; save on quit and recover on startup
  ;; (require 'emms-history)
  ;; (emms-history-load)
  (require 'emms-player-mpd)
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (setq emms-source-file-default-directory "~/Music/"
        emms-player-mpd-music-directory "~/Music/" ;; fixes a annoying bug
        emms-mode-line-format "「%s」"
        emms-playlist-buffer-name "*Music*"
        emms-info-asynchronously t
        emms-show-format "NP: %s"
        emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find ;; gnu "find" required
        emms-browser-covers 'emms-browser-cache-thumbnail-async
        emms-player-list '(emms-player-mpd) ;; mpv is too silent
        ;; emms-player-mpv-environment '("PULSE_PROP_media.role=music")
        ;; emms-player-mpv-parameters '("--quiet" "--really-quiet" "--no-audio-display" "--force-window=no" "--vo=xv");; can try --vo=null but does'nt play well at times
        emms-browser-thumbnail-small-size 64
        emms-browser-thumbnail-medium-size 128) ;; defaults are too big

  (emms-browser-make-filter "all" #'ignore)
  (emms-browser-make-filter "recent"
                            (lambda (track) (< 30
                                          (time-to-number-of-days
                                           (time-subtract (current-time)
                                                          (emms-info-track-file-mtime track))))))
  (emms-browser-set-filter (assoc "all" emms-browser-filters))
  ;; libre-fm
  ;; (emms-librefm-scrobbler-enable)

  (emms-playing-time-disable-display)

  (defvar emms-browser-info-title-format "%i%n")
  (defvar emms-browser-playlist-info-title-format
    emms-browser-info-title-format)

;; Show all tracks played in the last month:
(emms-browser-make-filter
 "last-month" (emms-browser-filter-only-recent 30))

;; After executing the above commands, you can use M-x emms-browser-show-all, emms-browser-show-80s, etc to toggle between different collections. Alternatively you can use ’<’ and ’>’ to cycle through the available filters.
;; The second argument to make-filter is a function which returns t if a single track should be filtered. You can write your own filter functions to check the type of a file, etc.
;; Show only tracks not played in the last year:

(emms-browser-make-filter "not-played"
 (lambda (track)
  (not (funcall (emms-browser-filter-only-recent 365) track))))

;To show a ’no cover’ image for albums ;;which don’t have a cover, add the
;;following code to your .emacs:
;; Suggested sizes are 100x100 for small, and 200x200 for medium.
  ;; (setq emms-browser-default-covers
  ;;   (list "/path/to/cover_small.jpg" nil nil))

  (defadvice emms-browser-next-mapping-type
      (after no-album (current-mapping))
    (when (eq ad-return-value 'info-album)
      (setq ad-return-value 'info-title)))

  (defun toggle-album-display ()
    (if (string= emms-browser-current-filter-name "singles")
        (ad-activate 'emms-browser-next-mapping-type)
      (ad-deactivate 'emms-browser-next-mapping-type)))

  (add-hook 'emms-browser-filter-changed-hook 'toggle-album-display)

  (defun my/tick-symbol (x)
    "Return a tick if X is true-ish."
    (if x "x" " "))

  (defun my/emms-player-status ()
    "Return the state of the EMMS player: `not-active', `playing', `paused' or `dunno'.

Modeled after `emms-player-pause'."
    (cond ((not emms-player-playing-p)
           ;; here we should return 'not-active.  The fact is that
           ;; when i change song, there is a short amount of time
           ;; where we are ``not active'', and the hydra is rendered
           ;; always during that short amount of time.  So we cheat a
           ;; little.
           'playing)

          (emms-player-paused-p
           (let ((resume (emms-player-get emms-player-playing-p 'resume))
                 (pause (emms-player-get emms-player-playing-p 'pause)))
             (cond (resume 'paused)
                   (pause  'playing)
                   (t      'dunno))))
          (t (let ((pause (emms-player-get emms-player-playing-p 'pause)))
               (if pause 'playing 'dunno)))))

  (defhydra hydra-emms (:hint nil)
    "
%(my/emms-player-status) %(emms-track-description (emms-playlist-current-selected-track))

^Volume^         ^Controls^       ^Playback^              ^Misc^
^^^^^^^^----------------------------------------------------------------
_d_: inc        _n_: next         _r_: repeat one [% s(my/tick-symbol emms-repeat-track)]     _t_oggle modeline
_o_: dec        _c_: prev         _R_: repeat all [% s(my/tick-symbol emms-repeat-playlist)]     _T_oggle only time
_v_: sysvol     _a_: seek bw      _#_: shuffle            _s_elect
_D_: sysvol+    _h_: seek fw      _%_: sort               _g_oto EMMS buffer
_O_: sysvol-    _SPC_: play/pause _m_: mute/unmute       _l_yrics
_O_: sysvol-    _SPC_: play/pause _m_: mute/unmute        _f_avorite
^ ^             _DEL_: restart                            _L_yrics select
  "
    ;; ("v" sndio-win-open :exit t)
    ;; ("d" emms-volume-raise) ;; TODO ;; needs fix to work with mpc
    ;; ("o" emms-volume-lower)
    ;; in the meanwhile
    ("d" simple-mpc-increase-volume)
    ("o" simple-mpc-decrease-volume)
    ("D" my/volume-increase)
    ("O" my/volume-decrease)
    ("m" +volume-toggle-mute)
    ("v" my/set-volume)
    ("n" emms-next)
    ("c" emms-previous) ;;blame my keyboard layout for not beign mnemonic
    ("a" emms-seek-backward)
    ("h" emms-seek-forward)
    ("SPC" emms-pause)
    ("DEL" (emms-player-seek-to 0))
    ("<backspace>" (emms-player-seek-to 0))
    ("r" emms-toggle-repeat-track)
    ("R" emms-toggle-repeat-playlist)
    ("#" emms-shuffle)
    ("%" emms-sort)
    ("t" (progn (+emms-toggle-time-display)
                (emms-mode-line-toggle)))
    ("T" +emms-toggle-time-display)
    ("s" +emms-select-song)
    ("g" (progn (emms)
                (with-current-emms-playlist
                  (emms-playlist-mode-center-current))))
    ("l" my/emms-current-lyrics :exit t)
    ("f" +emms-add-to-favorites)
    ("L" my/versuri-select :exit t)

    ("q" nil :exit t))

(map! :leader
      (:prefix ("y" . "EMMS audio player")
       :desc "Go to emms playlist" "p" #'emms-playlist-mode-go
       :desc "Emms Mpd start deamon" "s" #'+emms/mpd-start-music-daemon
       :desc "Emms Mpd update deamon" "u" #'+emms/mpc-update-database
       :desc "hydra emms body " "h" #'hydra-emms/body))

  (map! :map emms-playlist-mode-map
        :localleader
        "l" #'emms-toggle-repeat-playlist
        "p" #'emms-insert-playlist
        "i" #'emms-insert-file
        "t" #'emms-toggle-repeat-track
        "s" #'emms-playlist-save
        "m" #'emms-shuffle))


(defun +emms-bind-key (key command)
  (unless (local-key-binding key)
    (define-key (current-local-map) key command)))

(+emms-bind-key (kbd "C-c-e") 'hydra-emms/body)
