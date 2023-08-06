;; -*- no-byte-compile: t; -*-
;;; emacs/eww/packages.el

(package! eww :built-in t)
(when (modulep! +engine)
      (package! engine-mode))
(when (modulep! +langdetect)
      (package! language-detection))
(when (modulep! +shrface)
      (package! shrface))
(when (modulep! +highlight)
      (package! shr-tag-pre-highlight))
