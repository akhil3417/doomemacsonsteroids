;;; emacs/eww/config.el -*- lexical-binding: t; -*-

(use-package! eww
  :commands (eww)
  :config
  (add-hook! 'eww-after-render-hook #'eww--rename-buffer)
  (defadvice! eww-rename-buffer-a ()
    :after #'eww-back-url
    :after #'eww-forward-url
    (eww--rename-buffer))
  (setq eww-history-limit 150
        eww-restore-desktop t
        eww-desktop-remove-duplicates t
        eww-header-line-format nil
        eww-search-prefix "https://duckduckgo.com/html/?q="
        eww-browse-url-new-window-is-tab nil
        eww-form-checkbox-selected-symbol "[X]"
        eww-form-checkbox-symbol "[ ]"
        eww-retrieve-command nil) ;; NOTE set to 'nil in case it messes up with paragraph spacing
        ;; eww-retrieve-command '("wget" "--quiet" "--output-document=-")) ;; NOTE set to 'nil in case it messes up with paragraph spacing

  ;;NOTE following keybinds try to mimic qutebrowser
  (map! :map eww-mode-map
        :ni "C-<return>" #'+eww-open-in-other-window
        :n "yp" #'eww-copy-page-url
        :n "yl" #'link-hint-copy-link-at-point
        :n "yo" #'org-eww-copy-for-org-mode
        :n "+" #'+eww-increase-font-size
        :n "-" #'+eww-decrease-font-size
        :n "H"  #'eww-back-url
        :n "L"  #'eww-forward-url
        :n "r"  #'eww-reload
<<<<<<< HEAD
=======
        :n "I"  #'+eww-toogle-images
>>>>>>> feature-eww
        :n "gt" #'eww-switch-to-buffer
        :n "gT" #'eww-list-buffers
        :n "gb" #'eww-list-bookmarks
        :n "S" #'+eww-search-in-other-window
        :n "o"  #'eww-search-words
        :n "O"  #'+eww-visit-bookmark
        :n "m"  #'+eww-bookmark-page
        :n "D"  #'+eww-download-html
        :n "C-O"#'+open-link-at-point-in-external-browser
        :n "C-o"#'eww-browse-with-external-browser
        :n "C-e"#'+eww-browse-dwim
        :n "M"  #'browse-url-at-point-umpv
        :n "f" #'+eww-jump-to-url-on-page
        :n "C-f" #'+eww-visit-url-on-page
        [remap imenu] #'+eww-jump-to-url-on-page

        (:localleader
         :desc "external browser" "e" #'eww-browse-with-external-browser
         :desc "buffers" "b" #'eww-switch-to-buffer

         (:prefix ("t" . "toggle")
          :desc "readable" "r" #'eww-readable
          :desc "colors" "c" #'eww-toggle-colors
          :desc "fonts" "f" #'eww-toggle-fonts
          :desc "images" "i" #'eww-toggle-images))))

  ;; (map! :map eww-mode-map ;TODO add yank and kill history

(use-package! goto-addr
  :config
  (setq goto-address-url-face 'link
        goto-address-url-mouse-face 'highlight
        goto-address-mail-face nil
        goto-address-mail-mouse-face 'highlight))

(use-package! shr
  :config
  (setq shr-use-colors nil             ; t is bad for accessibility
        shr-use-fonts nil              ; t is not for me
        shr-max-image-proportion 0.6
        shr-image-animate nil          ; No GIFs, thank you!
        shr-width fill-column          ; check `prot-eww-readable'
        shr-max-width fill-column
        shr-discard-aria-hidden t
        shr-cookie-policy nil))

(use-package! url-cookie
  :config
  (setq url-cookie-untrusted-urls '(".*")))

(use-package! browse-url
  :commands (browse-url-at-point-umpv browse-url-umpv)
  :config
  (when IS-LINUX
    (defun browse-url-umpv (url &optional single)
      (start-process "mpv" nil (if single "mpv" "mpv")
                     (shell-quote-wildcard-pattern url)))

    (defun browse-url-mpv (url)
      (browse-url-umpv url t))

    (defun browse-url-at-point-umpv (&optional single)
      "Open link in mpv"
      (interactive "P")
      (let ((browse-url-browser-function
             (if single
                 (lambda (url &optional _new-window) (browse-url-umpv url t))
               #'browse-url-umpv)))
        (browse-url-at-point)))

    ))
