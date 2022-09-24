;;; completion/corfu/config.el -*- lexical-binding: t; -*-

;;
;;; Packages
(use-package! corfu
  :defer t
  :init
  (global-corfu-mode)
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        global-corfu-modes '((not
                              erc-mode
                              circe-mode
                              help-mode
                              gud-mode
                              vterm-mode)
                             t)
        corfu-cycle t
        corfu-separator (when (modulep! +orderless) ?\s)
        corfu-preselect 'valid
        corfu-count 16
        corfu-max-width 120
        corfu-preview-current 'insert
        corfu-on-exact-match nil
        corfu-quit-at-boundary (if (modulep! +orderless) 'separator t)
        corfu-quit-no-match (if (modulep! +orderless) 'separator t)
        ;; In the case of +tng, TAB should be smart regarding completion;
        ;; However, it should otherwise behave like normal, whatever normal was.
        tab-always-indent (if (modulep! +tng) 'complete tab-always-indent))
  (add-to-list 'completion-category-overrides `(lsp-capf (styles ,@completion-styles)))

  (map! :map corfu-mode-map
        :e "C-M-i" #'completion-at-point
        :i "C-SPC" #'completion-at-point
        :n "C-SPC" (cmd! (call-interactively #'evil-insert-state)
                         (call-interactively #'completion-at-point))
        :v "C-SPC" (cmd! (call-interactively #'evil-change)
                         (call-interactively #'completion-at-point)))
  (map! :unless (modulep! :editor evil)
        :map corfu-mode-map
        "C-M-i" #'completion-at-point)

  (add-hook! corfu-mode
    (defun +corfu-mode-unbinds ()
      ;; In `corfu-mode', unbind C-SPC from `global-map', so Emacs keeps searching.
      (make-local-variable 'global-map)
      (unbind-key "C-SPC" 'global-map)))

  (after! evil
    (add-hook 'evil-insert-state-exit-hook #'corfu-quit))

  (when (modulep! +icons)
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

  (map! :map corfu-map
        [return] #'corfu-insert
        "RET" #'corfu-insert)
  (when (modulep! +orderless)
    (map! :map corfu-map
          "C-SPC" #'corfu-insert-separator))
  (when (modulep! +tng)
    (map! :map corfu-map
          [tab] #'corfu-next
          [backtab] #'corfu-previous
          "TAB" #'corfu-next
          "S-TAB" #'corfu-previous)
    (let ((cmds-del (cmds! (and (modulep! +tng)
                                (> corfu--index -1)
                                (eq corfu-preview-current 'insert))
                           #'corfu-reset)))
      (map! :map corfu-map
            [backspace] cmds-del
            "DEL" cmds-del)))

  (after! vertico
    (map! :map corfu-map
          "M-m" #'+corfu-move-to-minibuffer
          (:when (modulep! :editor evil)
            "M-J" #'+corfu-move-to-minibuffer))))

(use-package! cape
  :defer t
  :init
  (add-hook! prog-mode
    (defun +corfu-add-cape-file-h ()
      (add-to-list 'completion-at-point-functions #'cape-file)))
  (add-hook! (org-mode markdown-mode)
    (defun +corfu-add-cape-elisp-block-h ()
      (add-to-list 'completion-at-point-functions #'cape-elisp-block)))
  (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible))

(use-package! corfu-terminal
  :when (not (display-graphic-p))
  :hook ((corfu-mode . corfu-terminal-mode)))

;;
;;; Extensions

(use-package! corfu-history
  :hook ((corfu-mode . corfu-history-mode))
  :config
  (after! savehist (add-to-list 'savehist-additional-variables 'corfu-history)))


(use-package! corfu-popupinfo
  :hook ((corfu-mode . corfu-popupinfo-mode))
  :config
  (setq corfu-popupinfo-delay '(0.5 . 1.0))
  (map! :map corfu-map
        "C-<up>" #'corfu-popupinfo-scroll-down
        "C-<down>" #'corfu-popupinfo-scroll-up
        "C-S-p" #'corfu-popupinfo-scroll-down
        "C-S-n" #'corfu-popupinfo-scroll-up
        "C-h" #'corfu-popupinfo-toggle)
  (map! :when (modulep! :editor evil)
        :map corfu-popupinfo-map
        ;; Reversed because popupinfo assumes opposite of what feels intuitive
        ;; with evil.
        "C-S-k" #'corfu-popupinfo-scroll-down
        "C-S-j" #'corfu-popupinfo-scroll-up))
