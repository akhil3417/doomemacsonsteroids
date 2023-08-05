;;; app/mpc/config.el -*- lexical-binding: t; -*-

;; don't want a tool bar and font changes
(defcustom mpc-frame-alist '((name . "MPC"))
  "Alist of frame parameters for the MPC frame."
  :type 'alist)

(use-package! simple-mpc
  :config
    (evil-define-key 'normal simple-mpc-mode-map
      "n" 'simple-mpc-next
      "p" 'simple-mpc-prev
      "r" 'simple-mpc-toggle-repeat
      "C" 'simple-mpc-clear-current-playlist
      "c" 'simple-mpc-view-current-playlist
      "d" '+delete-mpc-item-without-losing-place
      "D" '+mpd-remove-dupes
      "s" 'simple-mpc-query
      "S" 'simple-mpc-shuffle-current-playlist
      "t" 'simple-mpc-toggle
      ;; bd "SPC" 'simple-mpc-toggle
      "L" 'simple-mpc-load-playlist
      "a" 'simple-mpc-seek-backward
      "h" 'simple-mpc-seek-forward
      "P" '+save-playlist
      "J" '+mpc-shift-down
      "K" '+mpc-shift-up
      "e" '+evil-select-all
      "E" '+mpc-add-everything
      "T" 'hydra-mpc/body)

  (add-hook 'simple-mpc-mode-hook
            (lambda ()
              (+mpd-start)))

  (map! :leader
        (:prefix ("y" . "Audio players")
         :desc "Simple Mpc Hydra" "h" #'hydra-mpc/body
         :desc "Start Simple mpc" "m" #'simple-mpc)))

(defun +tick-symbol (x)
  "Return a tick if X is true-ish."
  (if x "x" " "))

(defhydra hydra-mpc (:hint nil)
   "
%(mpc-current-song)

 ^Volume^         ^Controls^       ^Playback^              ^Misc^
 ^^^^^^^^----------------------------------------------------------------
 _d_: inc        _n_: next         _S_: single     [% s(+tick-symbol +mpc-single)]     _t_oggle modeline
 _o_: dec        _c_: prev         _r_: repeat     [% s(+tick-symbol +mpc-repeat)]     _T_oggle only time
 _v_: sysvol     _a_: seek bw      _#_: shuffle            _s_elect
 _D_: sysvol+    _h_: seek fw      _%_: sort               _g_oto mpc buffer
 _O_: sysvol-    _SPC_: play/pause _m_: mute/unmute
 ^ ^             _DEL_: restart
  "
  ("d" simple-mpc-increase-volume)
  ("o" simple-mpc-decrease-volume)
  ("D" +volume-increase)
  ("O" +volume-decrease)
  ("m" +volume-toggle-mute)
  ("v" +set-volume)
  ("n" simple-mpc-next)
  ("c" simple-mpc-prev) ;;blame my keyboard layout for not being mnemonic
  ("h" simple-mpc-seek-forward)
  ("a" simple-mpc-seek-backward)
  ("SPC" simple-mpc-toggle)
  ("DEL" (simple-mpc-seek-internal(- 1000)));; yeah i know
  ("<backspace>" (simple-mpc-seek-internal(- 1000)));; yeah i know
  ("S" +mpc-toggle-single-cycle);; 'mpc-toggle-single  is present but
  ("r" simple-mpc-toggle-repeat)
  ("#" simple-mpc-shuffle-current-playlist)
  ;; ("%" (progn (simple-mpc-view-current-playlist)
  ;;             (simple-mpc-query-sort)))
  ("%" nil)
  ("t" (progn (+emms-toggle-time-display)
              (emms-mode-line-toggle)))
  ("T" +emms-toggle-time-display)
  ("s" +mpc-select-song)
  ("g" simple-mpc-view-current-playlist)
  ("q" nil :exit t))
