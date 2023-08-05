;;; app/mpc/config.el -*- lexical-binding: t; -*-

;; don't want a tool bar and font changes
(defcustom mpc-frame-alist '((name . "MPC"))
  "Alist of frame parameters for the MPC frame."
  :type 'alist)

(use-package! simple-mpc
  :config
  (defun +mpc-bindings ()
    (interactive)
    (evil-normal-state)
    (evil-local-set-key 'normal (kbd "n") 'simple-mpc-next)
    (evil-local-set-key 'normal (kbd "p") 'simple-mpc-prev)
    (evil-local-set-key 'normal (kbd "r") 'simple-mpc-toggle-repeat)
    (evil-local-set-key 'normal (kbd "C") 'simple-mpc-clear-current-playlist)
    (evil-local-set-key 'normal (kbd "c") 'simple-mpc-view-current-playlist)
    (evil-local-set-key 'normal (kbd "d") '+delete-mpc-item-without-losing-place)
    (evil-local-set-key 'normal (kbd "D") '+mpd-remove-dupes)
    (evil-local-set-key 'normal (kbd "s") 'simple-mpc-query)
    (evil-local-set-key 'normal (kbd "S") 'simple-mpc-shuffle-current-playlist)
    (evil-local-set-key 'normal (kbd "t") 'simple-mpc-toggle)
    ;; (evil-local-set-key 'normal (kbd "SPC" 'simple-mpc-toggle)
    (evil-local-set-key 'normal (kbd "L") 'simple-mpc-load-playlist)
    (evil-local-set-key 'normal (kbd "a") 'simple-mpc-seek-backward)
    (evil-local-set-key 'normal (kbd "h") 'simple-mpc-seek-forward)
    (evil-local-set-key 'normal (kbd "P") '+save-playlist)
    (evil-local-set-key 'normal (kbd "J") '+mpc-shift-down)
    (evil-local-set-key 'normal (kbd "K") '+mpc-shift-up)
    (evil-local-set-key 'normal (kbd "e") '+evil-select-all)
    (evil-local-set-key 'normal (kbd "E") '+mpc-add-everything)
    (evil-local-set-key 'normal (kbd "T") 'hydra-mpc/body))

  (add-hook 'simple-mpc-mode-hook
            (lambda ()
              (+mpd-start)
              (+mpc-bindings)))

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
 _D_: sysvol+    _h_: seek fw      _%_: sort               _g_oto EMMS buffer
 _O_: sysvol-    _SPC_: play/pause _m_: mute/unmute       _i_yrics
 ^ ^             _DEL_: restart                            _L_yrics select
  "
  ("d" simple-mpc-increase-volume)
  ("o" simple-mpc-decrease-volume)
  ("D" +volume-increase)
  ("O" +volume-decrease)
  ("m" +volume-toggle-mute)
  ("v" +set-volume)
  ("n" simple-mpc-next)
  ("c" simple-mpc-prev) ;;blame my keyboard layout for not being mnemonic
  ("a" simple-mpc-seek-forward)
  ("h" simple-mpc-seek-backward)
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
