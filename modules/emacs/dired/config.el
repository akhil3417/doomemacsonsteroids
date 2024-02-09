;;; emacs/dired/config.el -*- lexical-binding: t; -*-

(defvar +dired-dirvish-icon-provider 'nerd-icons
  "Icon provider to use for dirvish when the module is enabled.")

(use-package! dired
  :commands dired-jump
  :init
  (dirvish-override-dired-mode)
  (setq dired-dwim-target t  ; suggest a target for moving/copying intelligently
        dired-hide-details-hide-symlink-targets nil
        ;; don't prompt to revert, just do it
        dired-auto-revert-buffer #'dired-buffer-stale-p
        ;; Always copy/delete recursively
        dired-recursive-copies  'always
        dired-recursive-deletes 'top
        ;; Emacs 29 added mouse drag-and-drop support for Dired, the following settings will enable i
        dired-mouse-drag-files  t
        mouse-drag-and-drop-region-cross-program t

        dired-recursive-deletes 'top
        ;; Ask whether destination dirs should get created when copying/removing files.
        dired-create-destination-dirs 'ask
        ;; send deleted files to trash
        delete-by-moving-to-trash t
        ;; Where to store image caches
        image-dired-dir (concat doom-cache-dir "image-dired/")
        image-dired-db-file (concat image-dired-dir "db.el")
        image-dired-gallery-dir (concat image-dired-dir "gallery/")
        image-dired-temp-image-file (concat image-dired-dir "temp-image")
        image-dired-temp-rotate-image-file (concat image-dired-dir "temp-rotate-image")
        ;; Screens are larger nowadays, we can afford slightly larger thumbnails
        image-dired-thumb-size 150)
  :config
  (set-popup-rule! "^\\*image-dired"
    :slot 20 :size 0.8 :select t :quit nil :ttl 0)
  (set-evil-initial-state! 'image-dired-display-image-mode 'emacs)


  (let ((args (list "-ahl" "-v" "--group-directories-first")))
    (when (featurep :system 'bsd)
      ;; Use GNU ls as `gls' from `coreutils' if available. Add `(setq
      ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning
      ;; when not using GNU ls.
      (if-let (gls (executable-find "gls"))
          (setq insert-directory-program gls)
        ;; BSD ls doesn't support -v or --group-directories-first
        (setq args (list (car args)))))
    (setq dired-listing-switches (string-join args " "))

    (add-hook! 'dired-mode-hook
      (defun +dired-disable-gnu-ls-flags-maybe-h ()
        "Remove extraneous switches from `dired-actual-switches' when it's
uncertain that they are supported (e.g. over TRAMP or on Windows).

Fixes #1703: dired over TRAMP displays a blank screen.
Fixes #3939: unsortable dired entries on Windows."
        (when (or (file-remote-p default-directory)
                  (and (boundp 'ls-lisp-use-insert-directory-program)
                       (not ls-lisp-use-insert-directory-program)))
          (setq-local dired-actual-switches (car args))))))

  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil)

  (defadvice! +dired--no-revert-in-virtual-buffers-a (&rest args)
    "Don't auto-revert in dired-virtual buffers (see `dired-virtual-revert')."
    :before-while #'dired-buffer-stale-p
    (not (eq revert-buffer-function #'dired-virtual-revert)))

  (map! :map dired-mode-map
        ;; Kill all dired buffers on q
        :ng "q" #'+dired/quit-all
        ;; To be consistent with ivy/helm+wgrep integration
        "C-c C-e" #'wdired-change-to-wdired-mode))


(use-package! dired-rsync
  :general (dired-mode-map "C-c C-r" #'dired-rsync))


(use-package! diredfl
  :hook (dired-mode . diredfl-mode))
(use-package! ranger
  :when (modulep! +ranger)
  :after dired
  :init (setq ranger-override-dired t)
  :config
  (unless (file-directory-p image-dired-dir)
    (make-directory image-dired-dir))

  (set-popup-rule! "^\\*ranger" :ignore t)

  (defadvice! +dired--cleanup-header-line-a ()
    "Ranger fails to clean up `header-line-format' when it is closed, so..."
    :before #'ranger-revert
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (equal header-line-format '(:eval (ranger-header-line)))
            (setq header-line-format nil))))))

  (defadvice! +dired--cleanup-mouse1-bind-a ()
    "Ranger binds an anonymous function to mouse-1 after previewing a buffer
that prevents the user from escaping the window with the mouse. This command is
never cleaned up if the buffer already existed before ranger was initialized, so
we have to clean it up ourselves."
    :after #'ranger-setup-preview
    (when (window-live-p ranger-preview-window)
      (with-current-buffer (window-buffer ranger-preview-window)
        (local-unset-key [mouse-1]))))

  (defadvice! +dired--ranger-travel-a ()
    "Temporary fix for this function until ralesi/ranger.el#236 gets merged."
    :override #'ranger-travel
    (interactive)
    (let ((prompt "Travel: "))
      (cond
       ((bound-and-true-p helm-mode)
        (ranger-find-file (helm-read-file-name prompt)))
       ((bound-and-true-p ivy-mode)
        (ivy-read prompt 'read-file-name-internal
                  :matcher #'counsel--find-file-matcher
                  :action
                  (lambda (x)
                    (with-ivy-window
                     (ranger-find-file (expand-file-name x default-directory))))))
       ((bound-and-true-p ido-mode)
        (ranger-find-file (ido-read-file-name prompt)))
       (t
        (ranger-find-file (read-file-name prompt))))))

  (setq ranger-cleanup-on-disable t
        ranger-excluded-extensions '("mkv" "iso" "mp4")
        ranger-deer-show-details t
        ranger-max-preview-size 10
        ranger-show-literal nil
        ranger-hide-cursor nil))


(use-package! dired-aux
  :defer t
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))


(use-package! diredfl
  :hook (dired-mode . diredfl-mode)
  :hook (dirvish-directory-view-mode . diredfl-mode))


(use-package! dirvish
  :defer t
  :init (after! dired (dirvish-override-dired-mode))
  :hook (dired-mode . dired-omit-mode)
  :general (dired-mode-map "C-c C-r" #'dirvish-rsync)
  :after-call dired-noselect dired dired-jump
  :config
  (require 'dired-x)
  (setq dirvish-cache-dir (concat doom-cache-dir "dirvish/")
        dirvish-hide-details nil
        dirvish-attributes '(file-size collapse subtree-state)
        dirvish-mode-line-format
        '(:left (sort file-time symlink) :right (omit yank index))
        dired-omit-files (concat dired-omit-files "\\|^\\..*$"))
         ;; dirvish-use-header-line nil
         ;; dirvish-hide-cursor nil
  (when (modulep! +icons)
    (push +dired-dirvish-icon-provider dirvish-attributes))
  (set-popup-rule! "^ ?\\*Dirvish.*" :ignore t)
  (map! :map dirvish-mode-map
        :n  "?"   #'dirvish-dispatch
        :n  "q"   #'dirvish-quit
        :n  "h"   #'dired-up-directory
        :n  "l"   #'dired-find-file
        :ng "a"   #'dirvish-quick-access
        :n "b"    #'dirvish-goto-bookmark
        :n "z"    #'dirvish-show-history
        :ng "f"   #'dirvish-file-info-menu
        :n "F"    #'dirvish-toggle-fullscreen
        :ng "y"   #'dirvish-yank-menu
        :ng "s"   #'dirvish-quicksort
        :ng "TAB" #'dirvish-subtree-toggle
        :ng "M-t" #'dirvish-layout-toggle
        :ng "M-b" #'dirvish-history-go-backward
        :ng "M-f" #'dirvish-history-go-forward
        :ng "M-n" #'dirvish-narrow
        :ng "M-m" #'dirvish-mark-menu
        :ng "M-s" #'dirvish-setup-menu
        :ng "M-e" #'dirvish-emerge-menu
        :localleader
        "h" #'dired-omit-mode)
  (when (modulep! :ui tabs)
    (after! centaur-tabs
      (add-hook! 'dired-mode-hook 'centaur-tabs-local-mode)
      (add-hook! 'dirvish-directory-view-mode-hook 'centaur-tabs-local-mode)))
  (when (modulep! :ui vc-gutter)
    (push 'vc-state dirvish-attributes)))

;; Some keybindings for mouse:
;;     left click: expanding/collapsing a directory or opening a file
;;     right click: opening a file/directory
;;     middle click: opening a file/directory in new window

(setq mouse-1-click-follows-link nil)
(define-key dirvish-mode-map (kbd "<mouse-1>") 'dirvish-subtree-toggle-or-open)
(define-key dirvish-mode-map (kbd "<mouse-2>") 'dired-mouse-find-file-other-window)
(define-key dirvish-mode-map (kbd "<mouse-3>") 'dired-mouse-find-file)


(use-package! nerd-icons-dired
  :when (modulep! +icons)
  :unless (modulep! +dirvish)
  :hook (dired-mode . nerd-icons-dired-mode)
  :config
  (defadvice! +dired-disable-icons-in-wdired-mode-a (&rest _)
    :before #'wdired-change-to-wdired-mode
    (setq-local +wdired-icons-enabled (if nerd-icons-dired-mode 1 -1))
    (when nerd-icons-dired-mode
      (nerd-icons-dired-mode -1)))

  (defadvice! +dired-restore-icons-after-wdired-mode-a (&rest _)
    :after #'wdired-change-to-dired-mode
    (nerd-icons-dired-mode +wdired-icons-enabled)))


(use-package! dired-x
  :unless (modulep! +ranger)
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files
                "\\|^\\.DS_Store\\'"
                "\\|^\\.project\\(?:ile\\)?\\'"
                "\\|^\\.\\(?:svn\\|git\\)\\'"
                "\\|^\\.ccls-cache\\'"
                "\\|\\(?:\\.js\\)?\\.meta\\'"
                "\\|\\.\\(?:elc\\|o\\|pyo\\|swp\\|class\\)\\'"))
  ;; Disable the prompt about whether I want to kill the Dired buffer for a
  ;; deleted directory. Of course I do!
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  ;; Let OS decide how to open certain files
  (when-let (cmd (cond ((featurep :system 'macos) "open")
                       ((featurep :system 'linux) "xdg-open")
                       ((featurep :system 'windows) "start")))
    (setq dired-guess-shell-alist-user
          `(("\\.\\(?:docx\\|pdf\\|djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpe?g\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))
  (map! :map dired-mode-map
        :localleader
        "h" #'dired-omit-mode))


(use-package! fd-dired
  :when doom-projectile-fd-binary
  :defer t
  :init
  (global-set-key [remap find-dired] #'fd-dired)
  (set-popup-rule! "^\\*F\\(?:d\\|ind\\)\\*$" :ignore t))

(use-package! dired-aux
  :defer t
  :config
  (setq dired-create-destination-dirs 'ask
        dired-vc-rename-file t))

;;;###package dired-git-info
(map! :after dired
      :map (dired-mode-map ranger-mode-map)
      :ng ")" #'dired-git-info-mode)
(setq dgi-commit-message-format "%h %cs %s"
      dgi-auto-hide-details-p nil)
(after! wdired
  ;; Temporarily disable `dired-git-info-mode' when entering wdired, due to
  ;; reported incompatibilities.
  (defvar +dired--git-info-p nil)
  (defadvice! +dired--disable-git-info-a (&rest _)
    :before #'wdired-change-to-wdired-mode
    (setq +dired--git-info-p (bound-and-true-p dired-git-info-mode))
    (when +dired--git-info-p
      (dired-git-info-mode -1)))
  (defadvice! +dired--reactivate-git-info-a (&rest _)
    :after '(wdired-exit
             wdired-abort-changes
             wdired-finish-edit)
    (when +dired--git-info-p
      (dired-git-info-mode +1))))
