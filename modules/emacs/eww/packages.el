;; -*- no-byte-compile: t; -*-
;;; emacs/eww/packages.el

(package! eww :built-in t)
(when (modulep! +engine)
      package! engine-mode)
(when (modulep! +langdetec)
      package! language-detection)
