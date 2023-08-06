;;; emacs/eww/autoload.el -*- lexical-binding: t; -*-

;; a bunch of these things were borrowed from: Protesilaos Stavrou and remodeled for Doom
;; https://protesilaos.com/codelog/2021-03-25-emacs-eww

;;;###autoload
(defun eww--rename-buffer ()
  "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
                  (plist-get eww-data :url)
                (plist-get eww-data :title))))
    (rename-buffer (format "*%s # eww*" name) t)))

(defun eww--capture-url-on-page (&optional position)
  "Capture all the links on the current web page.

Return a list of strings.  Strings are in the form LABEL @ URL.
When optional argument POSITION is non-nil, include position info
in the strings too, so strings take the form
LABEL @ URL ~ POSITION."
  (let (links match)
    (save-excursion
      (goto-char (point-max))
      ;; NOTE 2021-07-25: The first clause in the `or' is meant to
      ;; address a bug where if a URL is in `point-min' it does not get
      ;; captured.
      (while (setq match (text-property-search-backward 'shr-url))
        (let* ((raw-url (prop-match-value match))
               (start-point-prop (prop-match-beginning match))
               (end-point-prop (prop-match-end match))
               (url (when (stringp raw-url)
                      (propertize raw-url 'face 'link)))
               (label (replace-regexp-in-string "\n" " " ; NOTE 2021-07-25: newlines break completion
                                                (buffer-substring-no-properties
                                                 start-point-prop end-point-prop)))
               (point start-point-prop)
               (line (line-number-at-pos point t))
               (column (save-excursion (goto-char point) (current-column)))
               (coordinates (propertize
                             (format "%d,%d (%d)" line column point)
                             'face 'shadow)))
          (when url
            (if position
                (push (format "%-15s ~ %s  @ %s"
                              coordinates label url)
                      links)
              (push (format "%s  @ %s"
                            label url)
                    links))))))
    links))

(defmacro eww-act-visible-window (&rest body)
  "Run BODY within narrowed-region.
If region is active run BODY within active region instead.
Return the value of the last form of BODY."
  `(save-restriction
     (if (use-region-p)
         (narrow-to-region (region-beginning) (region-end))
       (narrow-to-region (window-start) (window-end)))
     ,@body))

;;;###autoload
(defun +eww-jump-to-url-on-page (&optional arg)
  "Jump to URL position on the page using completion.

When called without ARG (\\[universal-argument]) get URLs only
from the visible portion of the buffer.  But when ARG is provided
consider whole buffer."
  (interactive "P")
  (when (derived-mode-p 'eww-mode)
    (let* ((links
            (if arg
                (eww--capture-url-on-page t)
              (eww-act-visible-window
               (eww--capture-url-on-page t))))
           (prompt-scope (if arg
                             (propertize "URL on the page" 'face 'warning)
                           "visible URL"))
           (prompt (format "Jump to %s: " prompt-scope))
           (selection (completing-read prompt links nil t))
           (position (replace-regexp-in-string "^.*(\\([0-9]+\\))[\s\t]+~" "\\1" selection))
           (point (string-to-number position)))
      (goto-char point)
      (recenter))))

;;;###autoload
(defun +eww-visit-url-on-page (&optional arg)
  "Visit URL from list of links on the page using completion.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive "P")
  (when (derived-mode-p 'eww-mode)
    (let* ((links (eww--capture-url-on-page))
           (selection (completing-read "Browse URL from page: " links nil t))
           (url (replace-regexp-in-string ".*@ " "" selection)))
      (eww url (when arg 4)))))

;;;###autoload
(defun +eww-browse-dwim (url &optional arg)
  "Visit a URL, maybe from `eww-prompt-history', with completion.

With optional prefix ARG (\\[universal-argument]) open URL in a
new eww buffer.  If URL does not look like a valid link, run a
web query using `eww-search-prefix'.

When called from an eww buffer, provide the current link as
\\<minibuffer-local-map>\\[next-history-element]."
  (interactive
   (let ((all-history (delete-dups
                       (append  eww-prompt-history)))
         (current-url (eww-current-url)))
     (list
      (completing-read "Run EWW on: " all-history
                       nil nil current-url 'eww-prompt-history current-url)
      (prefix-numeric-value current-prefix-arg))))
  (eww url arg))

;; TODO fix
;;;###autoload
(defun +eww-visit-bookmark (&optional arg)
  "Visit bookmarked URL.

With optional prefix ARG (\\[universal-argument]) open URL in a
new EWW buffer."
  (interactive "P")
  (eww-read-bookmarks)
  (let ((list (gensym)))
    (dolist (bookmark eww-bookmarks)
      (push (plist-get bookmark :url) list))
    (if eww-bookmarks
        (eww (completing-read "Visit EWW bookmark: " list)
        ;; (eww (ivy-completing-read "Visit EWW bookmark: " list) ;use ivy completion instead for this function
             (when arg 4))
      (user-error "No bookmarks"))))


;;;###autoload
(defun +eww-bookmark-page (title)
  "Add eww bookmark named with TITLE."
  (interactive
   (list
    (read-string "Set bookmark title: " (plist-get eww-data :title))))
  (plist-put eww-data :title title)
  (eww-add-bookmark))


(defvar +eww--punctuation-regexp "[][{}!@#$%^&*()_=+'\"?,.\|;:~`‘’“”]*"
  "Regular expression of punctionation that should be removed.")

(defun +eww--slug-no-punct (str)
  "Convert STR to a file name slug."
  (replace-regexp-in-string +eww--punctuation-regexp "" str))

(defun +eww--slug-hyphenate (str)
  "Replace spaces with hyphens in STR.
Also replace multiple hyphens with a single one and remove any
trailing hyphen."
  (replace-regexp-in-string
   "-$" ""
   (replace-regexp-in-string
    "-\\{2,\\}" "-"
    (replace-regexp-in-string "--+\\|\s+" "-" str))))

(defun +eww--sluggify (str)
  "Make STR an appropriate file name slug."
  (downcase (+eww--slug-hyphenate (+eww--slug-no-punct str))))


;; The `+common-shell-command-with-exit-code-and-output' function is
;; courtesy of Harold Carr, who also sent a patch that improved
;; `+eww-download-html' (from the `eww.el' library).
;;
;; More about Harold: <http://haroldcarr.com/about/>.
(defun +common-shell-command-with-exit-code-and-output (command &rest args)
  "Run COMMAND with ARGS.
Return the exit code and output in a list."
  (with-temp-buffer
    (list (apply 'call-process command nil (current-buffer) nil args)
          (buffer-string))))

;;;###autoload
(defun +eww-download-html (name)
  "Download web page and call the file with NAME."
  (interactive
   (list
    (+eww--sluggify
     (read-string "Set downloaded file name: " (plist-get eww-data :title)))))
  (let* ((path (thread-last eww-download-directory
                 (expand-file-name
                  (concat (format-time-string "%Y%m%d_%H%M%S") "--" name ".html"))))
         (out (+common-shell-command-with-exit-code-and-output
               "wget" "-q" (format "%s" (plist-get eww-data :url))
               "-O" (format "%s" (shell-quote-argument path)))))
    (if (= (car out) 0)
        (message "Downloaded page at %s" path)
      (message "Error downloading page: %s" (cdr out)))))

;;;###autoload
<<<<<<< HEAD
=======
(defun +eww-toggle-images ()
  "Toggle whether images are loaded and reload the current page fro cache."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload t)
  (message "Images are now %s"
           (if shr-inhibit-images "off" "on")))

;;;###autoload
>>>>>>> feature-eww
(defun +eww-open-in-other-window ()
  "Use `eww-open-in-new-buffer' in another window."
  (interactive)
  (other-window-prefix)       ; For emacs28 -- it's a hack, but why not?
  (eww-open-in-new-buffer))

;;;###autoload
(defun +eww-search-in-other-window ()
  "Use `eww-open-in-new-buffer' in another window."
  (interactive)
  (other-window-prefix)       ; For emacs28 -- it's a hack, but why not?
  (eww-search-words))

;;;###autoload
(defun +open-link-at-point-in-external-browser ()
  (interactive)
  (link-hint-copy-link-at-point)
  (let ((url (current-kill 0)))
    (browse-url-generic url)))

;;;###autoload
(defun +default-browser-eww()
  "Set eww to be default browser."
  (interactive)
  (setq engine/browser-function 'eww-browse-url;;  browse-url-default-browser open firefox
       browse-url-browser-function 'eww-browse-url
        browse-url-secondary-browser-function 'browse-url-generic))

;;;###autoload
(defun +default-browser-qutebrowser()
  (interactive)
  (setq engine/browser-function 'browse-url-generic;;  browse-url-default-browser open firefox
        browse-url-browser-function 'browse-url-generic))

;;;###autoload
(defun +default-browser-firefox()
  (interactive)
  (setq engine/browser-function 'browse-url-default-browser;;  browse-url-default-browser open firefox
        browse-url-browser-function 'browse-url-default-browser))


;;;###autoload
(defun +eww-increase-font-size ()
  (interactive)
  (if shr-use-fonts
      (let* ((cur (face-attribute 'shr-text :height nil))
             (cur (if (floatp cur) cur 1.0)))
        (set-face-attribute 'shr-text nil :height (+ cur 0.1)))
    (text-scale-increase 0.5)))

;;;###autoload
(defun +eww-decrease-font-size ()
  (interactive)
  (if shr-use-fonts
      (let* ((cur (face-attribute 'shr-text :height nil))
             (cur (if (floatp cur) cur 1.0)))
        (set-face-attribute 'shr-text nil :height (- cur 0.1)))
    (text-scale-decrease 0.5)))

;;;###autoload
(defun +open-link-with-mpv ()
  "Open the link under the point using mpv."
  (interactive)
  (save-excursion
    (let (url)
      (when (or (thing-at-point-looking-at "\\(https?\\):\\/\\/[^[:space:]]+")
                (thing-at-point-looking-at "\\(ftp\\):\\/\\/[^[:space:]]+"))
        (setq url (match-string-no-properties 0)))
      (when url
        (start-process "mpv" nil "mpv" url)))))
