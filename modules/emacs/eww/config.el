;;; emacs/eww/config.el -*- lexical-binding: t; -*-

(use-package! eww
  :commands (eww)
  :config
  (add-hook! 'eww-after-render-hook #'eww--rename-buffer)
  (defadvice! eww-rename-buffer-a ()
    :after #'eww-back-url
    :after #'eww-forward-url
    (eww--rename-buffer))

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

(use-package! engine-mode
  :when (modulep! +engine)
  :config
  ;; :browser 'browse-url-qutebrowser) this can be used to open specific urls with specific brouwser
  (defengine duckduckgo
    "https://lite.duckduckgo.com/lite/?q=%s&kf=-1&kz=-1&kq=-1&kv=-1&k1=-1&kp=-2&kaf=1&kd=-1"
    :keybinding "d")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "g")
  (defengine cern-gitlab
    "https://gitlab.cern.ch/search?search=%s"
    :keybinding "l")
  (defengine google
    "http://www.google.com/search?hl=en&ie=utf-8&oe=utf-8&q=%s"
    :keybinding "O")
  (defengine duckduckgo-first
    "https://duckduckgo.com/html?q=\\%s"
    :keybinding "f")
  (defengine google-maps
    "https://www.google.com/maps/search/%s/"
    :keybinding "M")
  (defengine openstreetmap
    "https://www.openstreetmap.org/search?query=%s"
    :keybinding "m")
  (defengine wordreference
    "https://www.wordreference.com/es/translation.asp?tranword=%s"
    :keybinding "r")
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w")
  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s"
    :keybinding "y"
    :browser 'browse-url-firefox)
  (defengine invidious
    "https://invidious.garudalinux.org/search?q=%s"
    :keybinding "v")
  (defengine url
    "https:%s"
    :keybinding "u")


  ;; # Search Engines

  (defengine bing
    "https://www.bing.com/search?q=%s"
    :keybinding "b")
  (defengine googlenews
    "https://news.google.com/search?q=%s"
    :keybinding "n")


  (defengine wiktionary
    "https://en.wiktionary.org/w/index.php?search=%s"
    :keybinding "D")

  ;; # Social Media
  (defengine reddit
    "https://www.reddit.com/search/?q=%s"
    :keybinding "r")

  ;; # Online Shopping
  (defengine amazon
    "https://www.amazon.in/s?k=%s"
    :keybinding "a"
    :browser 'browse-url-firefox)

  (defengine ebay
    "https://www.ebay.com/sch/i.html?&_nkw=%s"
    :keybinding "e")

  ;; # Linux
  (defengine archaur
    "https://aur.archlinux.org/packages/?O=0&K=%s"
    :keybinding "U")

  (defengine archpackages
    "https://archlinux.org/packages/?sort=&q=%s"
    :keybinding "P")

  (defengine archlinux
    "https://wiki.archlinux.org/index.php?search=%s"
    :keybinding "W")

  (defengine gitlab
    "https://gitlab.com/search?search=%s"
    :keybinding "G")

  (defengine opensource
    "https://opensource.google/projects/search?q=%s"
    :keybinding "o")

  (defengine sourceforge
    "https://sourceforge.net/directory/?q=%s"
    :keybinding "S")

  (defengine stackoverflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  ;;   (defengine craigslist
  ;; "https://www.craigslist.org/search/sss?query="
  ;; :keybinding "c")


  ;;   (defengine gumtree
  ;; "https://www.gumtree.com/search?search_category=all&q="
  ;; :keybinding "g")


  ;;   (defengine debian
  ;; "https://packages.debian.org/search?suite=default&section=all&arch=any&searchon=names&keywords="
  ;; :keybinding "D")

  ;;   (defengine brave
  ;; "https://search.brave.com/search?q="
  ;; :keybinding "b")

  ;;   (defengine gemini
  ;; "https://portal.mozz.us/gemini/geminispace.info/search%3F"
  ;; :keybinding "G")

  ;;   (defengine qwant
  ;; "https://www.qwant.com/?q="
  ;; :keybinding "q")

  ;;   (defengine swisscows
  ;; "https://swisscows.com/web?query="
  ;; :keybinding "s")

  ;;   (defengine yandex
  ;; "https://yandex.com/search/?text="
  ;; :keybinding "Y")

  ;; # Information/News
  ;;   (defengine bbc
  ;; "https://www.bbc.co.uk/search?q="
  ;; :keybinding "b")

  ;;   (defengine cnn
  ;; "https://www.cnn.com/search?q="
  ;; :keybinding "c")

  (engine/set-keymap-prefix (kbd "C-c w "))
  (engine-mode t))
