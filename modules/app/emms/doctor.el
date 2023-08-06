;;; app/emms/doctor.el -*- lexical-binding: t; -*-

(unless (or (executable-find "mpc")
            (executable-find "mpd"))
  (warn! "Couldn't find mpc or mpd executable; install mpc and mpd"))
