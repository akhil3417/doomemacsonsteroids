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

  (map! :map eww-mode-map
        :ni "C-<return>" #'+eww-open-in-other-window
        :n "yy" #'+eww-copy-current-url
        :n "zk" #'+eww-increase-font-size
        :n "zj" #'+eww-decrease-font-size
        [remap imenu] #'+eww-jump-to-url-on-page

        (:localleader
         :desc "external browser" "e" #'eww-browse-with-external-browser
         :desc "buffers" "b" #'eww-switch-to-buffer

         (:prefix ("t" . "toggle")
          :desc "readable" "r" #'eww-readable
          :desc "colors" "c" #'eww-toggle-colors
          :desc "fonts" "f" #'eww-toggle-fonts
          :desc "images" "i" #'eww-toggle-images)

         (:prefix ("y" . "copy")
          :desc "copy url" "y" #'+eww-copy-current-url
          :desc "copy for Org" "o" #'org-eww-copy-for-org-mode))))

