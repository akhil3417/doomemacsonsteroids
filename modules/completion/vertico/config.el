;;; completion/vertico/config.el -*- lexical-binding: t; -*-

(defvar +vertico-company-completion-styles '(basic partial-completion orderless)
  "Completion styles for company to use.

The completion/vertico module uses the orderless completion style by default,
but this returns too broad a candidate set for company completion. This variable
overrides `completion-styles' during company completion sessions.")

(defvar +vertico-consult-dir-container-executable "docker"
  "Command to call for listing container hosts.")

(defvar +vertico-consult-dir-container-args nil
  "Command to call for listing container hosts.")

;;
;;; Packages

(use-package! vertico
  :hook (doom-first-input . vertico-mode)
  :init
  (defadvice! +vertico-crm-indicator-a (args)
    :filter-args #'completing-read-multiple
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  :config
  (setq vertico-resize nil
        vertico-count 10
        vertico-cycle t)
  (setq-default completion-in-region-function
                (lambda (&rest args)
                  (apply (if vertico-mode
                             #'consult-completion-in-region
                           #'completion--in-region)
                         args)))

  (map! :when (modulep! :editor evil +everywhere)
        :map vertico-map
        (:when (modulep! +multiform)
          "M-V"   #'vertico-multiform-vertical
          "M-q"   #'vertico-multiform-grid
          "M-F"   #'vertico-multiform-flat
          "M-R"   #'vertico-multiform-reverse
          "M-U"   #'vertico-multiform-unobtrusive
          "C-l"   #'vertico-multiform-reverse)
        "M-RET" #'vertico-exit-input
        "C-o"   #'+vertico-quick-embark
        "C-a"   #'embark-act-with-completing-read
        "M-i"   #'+vertico-quick-insert
        "C-SPC" #'+vertico/embark-preview
        "C-j"   #'vertico-next
        "DEL"   #' vertico-directory-delete-char
        "M-DEL" #' vertico-directory-delete-word
        "C-w"   #' vertico-directory-delete-word
        "RET"   #' vertico-directory-enter
        ;; "C-j"   '(lambda () (interactive)
	;;            (if minibuffer--require-match
	;;                (minibuffer-complete-and-exit)
	;;              (exit-minibuffer)));;very useful , or maybe use vertico-exit-input
        "C-M-n" #'vertico-next-group
        "C-k"   #'vertico-previous
        "C-M-p" #'vertico-previous-group
        "C-h" (cmds! (eq 'file (vertico--metadata-get 'category)) #'vertico-directory-up)
        "M-l" (cmds! (eq 'file (vertico--metadata-get 'category)) #'+vertico/enter-or-preview))

  ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
  ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (map! :map vertico-map "DEL" #'vertico-directory-delete-char)
  ;; (setq vertico-buffer-display-action 'display-buffer-reuse-window)

  ;; These commands are problematic and automatically show the *Completions* buffer
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)
  (defadvice! +vertico--suppress-completion-help-a (fn &rest args)
    :around #'ffap-menu-ask
    (letf! ((#'minibuffer-completion-help #'ignore))
      (apply fn args))))

(when (modulep! +multiform)
  (use-package! vertico-multiform
    :commands vertico-multiform-mode
    :after vertico-flat
    ;; :bind (:map vertico-map
    ;;             ("M-q" . vertico-multiform-grid)
    ;;             ("C-l" . vertico-multiform-reverse)
    ;;             ("C-M-l" . embark-export))
    :init (vertico-multiform-mode 1)
    ;; :init (vertico-reverse-mode 1)
    :config
    (setq vertico-multiform-categories
          '((file my/vertico-grid-mode reverse)
            (jinx grid (vertico-grid-annotate . 20))
            (project-file my/vertico-grid-mode reverse)
            (imenu buffer)
            (consult-location buffer)
            (consult-grep buffer)
            (notmuch-result reverse)
            (minor-mode reverse)
            (reftex-label (:not unobtrusive))
            (embark-keybinding grid)
            (citar-reference reverse)
            (xref-location reverse)
            (history reverse)
            (url reverse)
            (consult-info buffer)
            (kill-ring reverse)
            (consult-compile-error reverse)
            (buffer flat (vertico-cycle . t))
            (t flat)))
    (setq vertico-multiform-commands
          '((jinx-correct reverse)
            (tab-bookmark-open reverse)
            (dired-goto-file unobtrusive)
            (load-theme my/vertico-grid-mode reverse)
            (consult-theme my/vertico-grid-mode reverse)
            (my/toggle-theme my/vertico-grid-mode reverse)
            (org-refile reverse)
            (org-agenda-refile reverse)
            (org-capture-refile reverse)
            (affe-find reverse)
            (execute-extended-command unobtrusive)
            (dired-goto-file flat)
            (consult-project-buffer flat)
            (consult-dir-maybe reverse)
            (consult-dir reverse)
            (consult-flymake reverse)
            (consult-history reverse)
            (consult-completion-in-region reverse)
            (consult-recoll buffer)
            (citar-insert-citation reverse)
            (completion-at-point reverse)
            (org-roam-node-find reverse)
            ;; (embark-completing-read-prompter reverse)
            ;; (embark-act-with-completing-read reverse)
            ;; (embark-bindings reverse)
            (consult-org-heading reverse)
            (consult-dff unobtrusive)
            (embark-find-definition reverse)
            (xref-find-definitions reverse)
            (my/eshell-previous-matching-input reverse)
            (tmm-menubar reverse)))

    (defun vertico-multiform-unobtrusive ()
      "Toggle the quiet display."
      (interactive)
      (vertico-multiform--define-display-toggle 'vertico-unobtrusive-mode)
      (if vertico-unobtrusive-mode
          (vertico-multiform--temporary-mode 'vertico-reverse-mode -1)
        (vertico-multiform--temporary-mode 'vertico-reverse-mode 1))))

  (after! vertico
    (require 'vertico-directory)
    (require 'vertico-repeat)
    (require 'vertico-reverse)
    (require 'vertico-flat)
    (require 'vertico-unobtrusive)
    (require 'vertico-buffer))

  (use-package! vertico-grid
    :after vertico
    ;; :bind (:map vertico-map ("M-q" . vertico-grid-mode))
    :config
    (defvar my/vertico-count-orig vertico-count)
    (define-minor-mode my/vertico-grid-mode
      "Vertico-grid display with modified row count."
      :global t :group 'vertico
      (cond
       (my/vertico-grid-mode
        (setq my/vertico-count-orig vertico-count)
        (setq vertico-count 4)
        (vertico-grid-mode 1))
       (t (vertico-grid-mode 0)
          (setq vertico-count my/vertico-count-orig))))
    (setq vertico-grid-separator "    ")
    (setq vertico-grid-lookahead 50)))


(use-package! orderless
  :after-call doom-first-input-hook
  :config
  (defadvice! +vertico--company-capf--candidates-a (fn &rest args)
    "Highlight company matches correctly, and try default completion styles before
orderless."
    :around #'company-capf--candidates
    (let ((orderless-match-faces [completions-common-part])
          (completion-styles +vertico-company-completion-styles))
      (apply fn args)))

  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Annotation
     ((string-prefix-p "&" pattern) `(orderless-annotation . ,(substring pattern 1)))
     ((string-suffix-p "&" pattern) `(orderless-annotation . ,(substring pattern 0 -1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))
  (add-to-list
   'completion-styles-alist
   '(+vertico-basic-remote
     +vertico-basic-remote-try-completion
     +vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in
        ;; find-file etc.
        completion-category-overrides '((file (styles +vertico-basic-remote orderless partial-completion)))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator #'orderless-escapable-split-on-space)
  ;; ...otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil))


(use-package! consult
  :defer t
  :preface
  (define-key!
    [remap bookmark-jump]                 #'consult-bookmark
    [remap evil-show-marks]               #'consult-mark
    [remap evil-show-jumps]               #'+vertico/jump-list
    [remap evil-show-registers]           #'consult-register
    [remap goto-line]                     #'consult-goto-line
    [remap imenu]                         #'consult-imenu
    [remap Info-search]                   #'consult-info
    [remap locate]                        #'consult-locate
    [remap load-theme]                    #'consult-theme
    [remap man]                           #'consult-man
    [remap recentf-open-files]            #'consult-recent-file
    [remap switch-to-buffer]              #'consult-buffer
    [remap switch-to-buffer-other-window] #'consult-buffer-other-window
    [remap switch-to-buffer-other-frame]  #'consult-buffer-other-frame
    [remap yank-pop]                      #'consult-yank-pop
    [remap persp-switch-to-buffer]        #'+vertico/switch-workspace-buffer)
  :config
  (defadvice! +vertico--consult-recentf-a (&rest _args)
    "`consult-recent-file' needs to have `recentf-mode' on to work correctly.
`consult-buffer' needs `recentf-mode' to show file candidates."
    :before (list #'consult-recent-file #'consult-buffer)
    (recentf-mode +1))

  (setq consult-project-function #'doom-project-root
        consult-narrow-key "<"
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1
        consult-fd-args
        '((if (executable-find "fdfind" 'remote) "fdfind" "fd")
          "--color=never"
          ;; https://github.com/sharkdp/fd/issues/839
          "--full-path --absolute-path"
          "--hidden --exclude .git"
          (if (featurep :system 'windows) "--path-separator=/")))

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key "C-SPC")
  (when (modulep! :config default)
    (consult-customize
     +default/search-project +default/search-other-project
     +default/search-project-for-symbol-at-point
     +default/search-cwd +default/search-other-cwd
     +default/search-notes-for-symbol-at-point
     +default/search-emacsd
     :preview-key "C-SPC"))
  (consult-customize
   consult-theme
   :preview-key (list "C-SPC" :debounce 0.5 'any))
  (when (modulep! :lang org)
    (defvar +vertico--consult-org-source
      (list :name     "Org Buffer"
            :category 'buffer
            :narrow   ?o
            :hidden   t
            :face     'consult-buffer
            :history  'buffer-name-history
            :state    #'consult--buffer-state
            :new
            (lambda (name)
              (with-current-buffer (get-buffer-create name)
                (insert "#+title: " name "\n\n")
                (org-mode)
                (consult--buffer-action (current-buffer))))
            :items
            (lambda ()
              (mapcar #'buffer-name
                      (if (featurep 'org)
                          (org-buffer-list)
                        (seq-filter
                         (lambda (x)
                           (eq (buffer-local-value 'major-mode x) 'org-mode))
                         (buffer-list)))))))
    (add-to-list 'consult-buffer-sources '+vertico--consult-org-source 'append)))


(use-package! consult-dir
  :defer t
  :init
  (map! [remap list-directory] #'consult-dir
        (:after vertico
         :map vertico-map
         "C-x C-d" #'consult-dir
         "C-x C-j" #'consult-dir-jump-file))
  :config
  ;; DEPRECATED: Remove when Doom core replaces projectile with project.el
  (setq consult-dir-project-list-function #'consult-dir-projectile-dirs)

  (when (modulep! :tools docker)
    ;; TODO: Replace with `tramp-container--completion-function' when we drop
    ;;   support for <29
    (defun +vertico--consult-dir-container-hosts (host)
      "Get a list of hosts from HOST."
      (cl-loop for line in (cdr
                            (ignore-errors
                              (apply #'process-lines +vertico-consult-dir-container-executable
                                     (append +vertico-consult-dir-container-args (list "ps")))))
               for cand = (split-string line "[[:space:]]+" t)
               collect (format "/%s:%s:/" host (car (last cand)))))

    (defun +vertico--consult-dir-podman-hosts ()
      (let ((+vertico-consult-dir-container-executable "podman"))
        (+vertico--consult-dir-container-hosts "podman")))

    (defun +vertico--consult-dir-docker-hosts ()
      (let ((+vertico-consult-dir-container-executable "docker"))
        (+vertico--consult-dir-container-hosts "docker")))

    (defvar +vertico--consult-dir-source-tramp-podman
      `(:name     "Podman"
        :narrow   ?p
        :category file
        :face     consult-file
        :history  file-name-history
        :items    ,#'+vertico--consult-dir-podman-hosts)
      "Podman candidate source for `consult-dir'.")

    (defvar +vertico--consult-dir-source-tramp-docker
      `(:name     "Docker"
        :narrow   ?d
        :category file
        :face     consult-file
        :history  file-name-history
        :items    ,#'+vertico--consult-dir-docker-hosts)
      "Docker candidate source for `consult-dir'.")

    (add-to-list 'consult-dir-sources '+vertico--consult-dir-source-tramp-podman t)
    (add-to-list 'consult-dir-sources '+vertico--consult-dir-source-tramp-docker t))

  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-ssh t)
  (add-to-list 'consult-dir-sources 'consult-dir--source-tramp-local t))

(use-package! consult-flycheck
  :when (and (modulep! :checkers syntax)
             (not (modulep! :checkers syntax +flymake)))
  :after (consult flycheck))

(use-package! consult-yasnippet
  :when (modulep! :editor snippets)
  :defer t
  :init (map! [remap yas-insert-snippet] #'consult-yasnippet))


(use-package! embark
  :defer t
  :init
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
  (map! [remap describe-bindings] #'embark-bindings
        "C-;"               #'embark-act  ; to be moved to :config default if accepted
        (:map minibuffer-local-map
         "C-a"               #'embark-act-with-completing-read
         "C-S-o"             #'embark-minimal-act
         "C-M-o"             #'embark-minimal-act-noexit
         "C-b"               #'embark-become
         "C-;"               #'embark-act
         "C-c C-o"           #'embark-export
         "C-c C-l"           #'embark-collect
         :desc "Export to writable buffer" "C-c C-e" #'+vertico/embark-export-write)
        (:leader
         :desc "Actions" "a" #'embark-act)) ; to be moved to :config default if accepted
  :config
  (require 'consult)

  (defun +embark-act-with-completing-read (&optional arg)
    (interactive "P")
    (let* ((embark-prompter 'embark-completing-read-prompter)
           (act (propertize "Act" 'face 'highlight))
           (embark-indicators '(embark-minimal-indicator)))
      (embark-act arg)))

  (set-popup-rule! "^\\*Embark Export:" :size 0.35 :ttl 0 :quit nil)

  (after! which-key
    (defadvice! +vertico--embark-which-key-prompt-a (fn &rest args)
      "Hide the which-key indicator immediately when using the completing-read prompter."
      :around #'embark-completing-read-prompter
      (which-key--hide-popup-ignore-command)
      (let ((embark-indicators
             (remq #'embark-which-key-indicator embark-indicators)))
        (apply fn args)))
    (cl-nsubstitute #'+vertico-embark-which-key-indicator #'embark-mixed-indicator embark-indicators))

  ;; add the package! target finder before the file target finder,
  ;; so we don't get a false positive match.
  (let ((pos (or (cl-position
                  'embark-target-file-at-point
                  embark-target-finders)
                 (length embark-target-finders))))
    (cl-callf2
        cons
        '+vertico-embark-target-package-fn
        (nthcdr pos embark-target-finders)))
  (defvar-keymap +vertico/embark-doom-package-map
    :doc "Keymap for Embark package actions for packages installed by Doom."
    :parent embark-general-map
    "h" #'doom/help-packages
    "b" #'doom/bump-package
    "c" #'doom/help-package-config
    "u" #'doom/help-package-homepage)
  (setf (alist-get 'package embark-keymap-alist) #'+vertico/embark-doom-package-map)
  (map! (:map embark-file-map
         :desc "Open target with sudo"        "s"   #'doom/sudo-find-file
         (:when (modulep! :tools magit)
           :desc "Open magit-status of target" "g"   #'+vertico/embark-magit-status)
         (:when (modulep! :ui workspaces)
           :desc "Open in new workspace"       "TAB" #'+vertico/embark-open-in-new-workspace
           :desc "Open in new workspace"       "<tab>" #'+vertico/embark-open-in-new-workspace))))


(use-package! marginalia
  :hook (doom-first-input . marginalia-mode)
  :init
  (map! :map minibuffer-local-map
        :desc "Cycle marginalia views" "M-A" #'marginalia-cycle)
  :config
  (when (modulep! +icons)
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))
  (advice-add #'marginalia--project-root :override #'doom-project-root)
  (pushnew! marginalia-command-categories
            '(+default/find-file-under-here . file)
            '(doom/find-file-in-emacsd . project-file)
            '(doom/find-file-in-other-project . project-file)
            '(doom/find-file-in-private-config . file)
            '(doom/describe-active-minor-mode . minor-mode)
            '(flycheck-error-list-set-filter . builtin)
            '(persp-switch-to-buffer . buffer)
            '(projectile-find-file . project-file)
            '(projectile-recentf . project-file)
            '(projectile-switch-to-buffer . buffer)
            '(projectile-switch-project . project-file)))


(use-package! wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))


(use-package! vertico-posframe
  :when (modulep! +childframe)
  :hook (vertico-mode . vertico-posframe-mode)
  :config
  (add-hook 'doom-after-reload-hook #'posframe-delete-all))

;; From https://github.com/minad/vertico/wiki#candidate-display-transformations-custom-candidate-highlighting
;;
;; Uses `add-face-text-property' instead of `propertize' unlike the above snippet
;; because `'append' is necessary to not override the match font lock
;; See: https://github.com/minad/vertico/issues/389
(use-package! vertico-multiform
  :hook (vertico-mode . vertico-multiform-mode)
  :config
  (defvar +vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun +vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (when (string-suffix-p "/" file)
      (add-face-text-property 0 (length file) 'marginalia-file-priv-dir 'append file))
    file)

  (defun +vertico-highlight-enabled-mode (cmd)
    "If MODE is enabled, highlight it as font-lock-constant-face."
    (let ((sym (intern cmd)))
      (with-current-buffer (nth 1 (buffer-list))
        (if (or (eq sym major-mode)
                (and
                 (memq sym minor-mode-list)
                 (boundp sym)
                 (symbol-value sym)))
            (add-face-text-property 0 (length cmd) 'font-lock-constant-face 'append cmd)))
      cmd))

  (add-to-list 'vertico-multiform-categories
               '(file
                 (+vertico-transform-functions . +vertico-highlight-directory)))
  (add-to-list 'vertico-multiform-commands
               '(execute-extended-command
                 (+vertico-transform-functions . +vertico-highlight-enabled-mode))))
