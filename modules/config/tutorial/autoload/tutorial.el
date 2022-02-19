;;; config/tutorial/autoload/tutorial.el -*- lexical-binding: t; -*-

(defvar doom-tutorial-hist-file
  (expand-file-name "tutorial-progress.el" doom-cache-dir)
  "Directory where tutorial progress information is saved.")

(defvar doom-tutorial--progress
  (when (file-exists-p doom-tutorial-hist-file)
    (with-temp-buffer
      (insert-file-contents doom-tutorial-hist-file)
      (read (current-buffer))))
  "An alist of tutorials and progress information.")

(defun doom-tutorial--save-progress ()
  "Write `doom-tutorial--progress' to `doom-tutorial-hist-file'."
  (with-temp-buffer
    (insert
     ";; -*- mode: emacs-lisp -*-\n"
     ";; Tutorial progress file, automatically generated by `doom-tutorial--save-progress'.\n"
     "\n")
    (let ((print-length nil)
          (print-level nil)
          (print-quoted t))
      (prin1 doom-tutorial--progress
             (current-buffer)))
    (insert ?\n)
    (condition-case err
        (write-region (point-min) (point-max) doom-tutorial-hist-file nil
                      (unless (called-interactively-p 'interactive) 'quiet))
      (file-error
       (lwarn '(doom-tutorial-hist-file) :warning "Error writing `%s': %s"
              doom-tutorial-hist-file (caddr err))))))

(defvar doom-tutorial--registered nil
  "An alist of registered tutorials.")

(defvar doom-tutorial--test-interval 0.5
  "How often to try the test function.")

(defun doom-tutorial-run (name)
  "Run the tutorial NAME."
  (unless (and (plist-get (cdr (assoc name doom-tutorial--progress)) :complete)
               (not (yes-or-no-p "You have already completed this tutorial, would you like to do it again?")))
    (when (plist-get (cdr (assoc name doom-tutorial--progress)) :complete)
      (plist-put (cdr (assoc name doom-tutorial--progress)) :complete nil)
      (plist-put (cdr (assoc name doom-tutorial--progress)) :page 0))
    (doom-tutorial-quit)
    (when-let ((tutorial (cdr (assoc name doom-tutorial--registered))))
      (eval (plist-get tutorial :setup)))
    (doom-tutorial-load-page name)
    (setq doom-tutorial--test-timer
          (run-at-time t doom-tutorial--test-interval #'doom-tutorial--check-test))))

(defun doom-tutorial-run-maybe (name)
  (unless (or (plist-get (cdr (assoc name doom-tutorial--progress)) :skipped)
              (plist-get (cdr (assoc name doom-tutorial--progress)) :complete))
    (pcase (read-char-choice
            (format "Do you want to run the %s tutorial? (y)es/(l)ater/(n)ever: "
                    (propertize (symbol-name name) 'face 'bold))
            '(?y ?l ?n))
      (?y (doom-tutorial-run name))
      (?n (plist-put (cdr (assoc name doom-tutorial--progress)) :skipped nil)))))

(defun doom-tutorial-normalise-plist (somelist)
  (cdr (cl-reduce
        (lambda (result new)
          (if (keywordp new)
              (progn (push new result)
                     (push nil result))
            (push new (car result)))
          result)
        (nreverse somelist)
        :initial-value (list nil))))

;;;###autoload
(defmacro define-tutorial! (name &optional docstring &rest body)
  (declare (doc-string 2) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let ((parameters (doom-tutorial-normalise-plist body)))
    (when (plist-get parameters :setup)
      (plist-put parameters :setup
                 (append (list #'progn) (plist-get parameters :setup))))
    (when (plist-get parameters :teardown)
      (plist-put parameters :teardown
                 (append (list #'progn) (plist-get parameters :teardown))))
    (when (plist-get parameters :pages)
      (plist-put parameters :pages
                 (mapcar (lambda (page)
                           (if (eq 'page (car page))
                               (eval `(doom-tutorial-page! ,@(cdr page)))
                             page))
                         (plist-get parameters :pages))))
    `(progn
       (defun ,(intern (format "doom-tutorial-%s" name)) (&optional autotriggered)
         ,docstring
         (interactive "p")
         (if autotriggered
             (doom-tutorial-run ',name)
           (doom-tutorial-run-maybe ',name)))
       (doom-tutorial-register ',name ',parameters))))

(defun doom-tutorial-register (name parameters)
  (push (cons name parameters) doom-tutorial--registered)
  (unless (assoc name doom-tutorial--progress)
    (push (list name :skipped nil :complete nil :page 0)
          doom-tutorial--progress))
  (dolist (target (plist-get parameters :triggers))
    (advice-add target :after (intern (format "doom-tutorial-%s" name))))
  (dolist (filepattern (plist-get parameters :file-triggers))
    (add-to-list 'doom-tutorials--file-triggers (cons (eval filepattern) name))))

;;;###autoload
(defun doom-tutorial-load-modules ()
  (let (loaded-tutorials)
    (maphash (lambda (key _plist)
               (let ((tutorial-file (doom-module-path (car key) (cdr key) "tutorial.el")))
                 (when (file-exists-p tutorial-file)
                   (push (cdr key) loaded-tutorials)
                   (load tutorial-file 'noerror 'nomessage))))
             doom-modules)
    loaded-tutorials))

(defmacro doom-tutorial-page! (&rest body)
  (let ((parameters (doom-tutorial-normalise-plist body)))
    (dolist (strparam '(:title :instructions :template))
      (if-let ((paramvalue (plist-get parameters strparam)))
          (plist-put parameters strparam
                     (if (cl-every #'stringp paramvalue)
                         (apply #'concat paramvalue)
                       `(lambda () (concat ,@paramvalue))))))
    (dolist (funparam '(:setup :test))
      (when-let ((paramvalue (plist-get parameters funparam)))
        (plist-put parameters funparam
                   (cond
                    ((functionp paramvalue) paramvalue)
                    ((consp paramvalue) `(lambda () ,@paramvalue))
                    (_ (error "%s is invalid. %S" funparam paramvalue))))))
    `(list ,@parameters)))

(defun doom-tutorial--current-page (name)
  (plist-get (cdr (assoc name doom-tutorial--progress)) :page))

(defun doom-tutorial--set-page (name page)
  (plist-put (cdr (assoc name doom-tutorial--progress)) :page page)
  page)

(defvar doom-tutorial--name nil)
(defvar doom-tutorial--test nil)

(defun doom-tutorial-load-page (name &optional page)
  "Load page PAGE of the tutorial NAME."
  (let* ((page (or (and page
                        (doom-tutorial--set-page name page))
                   (doom-tutorial--current-page name)))
         (pages (length
                 (plist-get (cdr (assoc name doom-tutorial--registered))
                            :pages)))
         (content (nth page
                       (plist-get (cdr (assoc name doom-tutorial--registered))
                                  :pages))))
    (if (>= page pages)
        (doom-tutorial--complete name)
      (let ((instructions (or (plist-get content :instructions) ""))
            (title (or (plist-get content :title) "")))
        (with-current-buffer doom-tutorial--instructions-buffer-name
          (setq-local header-line-format
                      (propertize
                       (format "Step %s/%s:	%s"
                               (1+ page) pages title)
                       'face '(bold org-document-title)))
          (doom-tutorial--set-info-modeline page pages)
          (setq-local doom-tutorial--name name)
          (setq-local doom-tutorial--test (plist-get content :test))
          (setq doom-tutorial--cmd-log nil)
          (with-silent-modifications
            (erase-buffer)
            (insert ?\n)
            (insert
             (cond ((stringp instructions) instructions)
                   ((functionp instructions) (funcall instructions)))))))
      (let ((template (plist-get content :template))
            (setup (plist-get content :setup)))
        (with-current-buffer doom-tutorial--scratchpad-buffer-name
          ;; Reapply local variables values, in case the got cleared
          ;; (e.g. by mode changing).
          (setq-local header-line-format
                      (propertize "Scratch pad" 'face '(bold org-document-title)))
          (add-hook 'pre-command-hook #'doom-tutorial--log-cmd nil t)
          (when template
            (erase-buffer)
            (insert
             (cond
              ((stringp template) template)
              ((functionp template) (funcall template)))))
          (when setup (funcall setup)))))))

(defun doom-tutorial--set-info-modeline (page npages)
  (with-current-buffer doom-tutorial--instructions-buffer-name
    (setq-local mode-line-format
                (with-temp-buffer
                  (insert " ")
                  (if (> page 0)
                      (insert-text-button
                       "previous"
                       'action (lambda (_) (doom-tutorial-last-page))
                       'face 'org-link
                       'help-echo "Go to previous step"
                       'follow-link t)
                    (insert
                     (propertize "previous" 'face 'font-lock-comment-face)))
                  (insert " / ")
                  (if (> npages (1+ page))
                      (insert-text-button
                       "next"
                       'action (lambda (_) (doom-tutorial-next-page))
                       'face 'org-link
                       'help-echo "Go to next step"
                       'follow-link t)
                    (insert
                     (propertize "next" 'face 'font-lock-comment-face)))
                  (insert " step")
                  (buffer-string)))))

(defun doom-tutorial--complete (name)
  (with-current-buffer doom-tutorial--instructions-buffer-name
    (setq-local header-line-format
                (propertize "Finished!" 'face '(bold org-document-title)))
    (setq-local doom-tutorial--test nil)
    (with-silent-modifications
      (erase-buffer)
      (insert ?\n
              (format "You have completed the %s tutorial!" name))))
  (plist-put (cdr (assoc name doom-tutorial--progress)) :complete t))

(defun doom-tutorial-last-page ()
  (let* ((name (buffer-local-value
                'doom-tutorial--name
                (get-buffer
                 doom-tutorial--instructions-buffer-name))))
    (doom-tutorial--set-page name (1- (doom-tutorial--current-page name)))
    (doom-tutorial-load-page name)))

(defun doom-tutorial-next-page ()
  (let* ((name (buffer-local-value
                'doom-tutorial--name
                (get-buffer
                 doom-tutorial--instructions-buffer-name))))
    (doom-tutorial--set-page name (1+ (doom-tutorial--current-page name)))
    (doom-tutorial-load-page name))
  (doom-tutorial--save-progress))

(defvar doom-tutorial--test-timer nil)

(defun doom-tutorial--check-test ()
  (let ((test (buffer-local-value
               'doom-tutorial--test
               (get-buffer
                doom-tutorial--instructions-buffer-name))))
    (with-current-buffer doom-tutorial--scratchpad-buffer-name
      (when (and test (funcall test))
        (doom-tutorial-next-page)))))

(defvar doom-tutorial--cmd-log nil)

(defvar doom-tutorial--cmd-log-ignore
  '(nil self-insert-command backward-char forward-char
        delete-char delete-backward-char backward-delete-char
        backward-delete-char-untabify
        universal-argument universal-argument-other-key
        universal-argument-minus universal-argument-more
        beginning-of-line end-of-line recenter
        move-end-of-line move-beginning-of-line
        handle-switch-frame
        newline previous-line next-line
        mwheel-scroll mouse-set-point mouse-set-region
        evil-mouse-drag-region)
  "A list of commands which should not be logged.")

(defun doom-tutorial--log-cmd (&optional cmd)
  (when (string= (buffer-name) doom-tutorial--scratchpad-buffer-name)
    (let ((cmd (or cmd this-command))
          (keys (this-command-keys)))
      (cond
       ((memq cmd doom-tutorial--cmd-log-ignore)
        nil)
       ((eq cmd (caar doom-tutorial--cmd-log))
        (setf (cadar doom-tutorial--cmd-log)
              (1+ (cadar doom-tutorial--cmd-log)))
        (with-current-buffer doom-tutorial--cmd-log-buffer-name
          (goto-char (point-max))
          (delete-region (line-beginning-position 0) (point)))
        (doom-tutorial--log-cmd-insert))
       (t
        (push (list cmd 1 keys (current-time))
              doom-tutorial--cmd-log)
        (doom-tutorial--log-cmd-insert))))))

(defun doom-tutorial--log-cmd-insert (&optional entry)
  (let ((entry (or entry (car doom-tutorial--cmd-log))))
    (with-current-buffer doom-tutorial--cmd-log-buffer-name
      (goto-char (point-max))
      (insert
       (propertize (format-time-string "%H:%M:%S" (cadddr entry))
                   'face '((:height 0.9) font-lock-type-face))
       " ")
      (let ((action (car entry)))
        (cond
         ((symbolp action)
          (insert-text-button
           (symbol-name action)
           'action `(lambda (_) (helpful-callable ',(car entry)))
           'face 'underline
           'help-echo "Open help page"
           'follow-link t))
         ((functionp action)
          (insert (propertize "anonymous function" 'face 'italic)))))
      (when-let ((keys (caddr entry)))
        (insert
         (propertize (format " (%s)" (key-description keys))
                     'face 'font-lock-keyword-face)))
      (when (> (cadr entry) 1)
        (insert
         (propertize (format " × %d" (cadr entry))
                     'face 'font-lock-doc-face)))
      (insert ?\n)
      (set-window-point (get-buffer-window doom-tutorial--cmd-log-buffer-name) (point-max)))))

(defun doom-tutorial-cmd-log-startswith (sequence)
  "Check if the command log most recently recorded SEQUENCE of symbols."
  (and (>= (length doom-tutorial--cmd-log) (length sequence))
       (equal (reverse sequence)
              (cl-subseq (mapcar #'car doom-tutorial--cmd-log) 0 (length sequence)))))

(defvar doom-tutorial-workspace-name "*tutorial*")
(defvar doom-tutorial--old-windowconf nil)

(defvar doom-tutorial--scratchpad-buffer-name "*tutorial scratchpad*")
(defvar doom-tutorial--scratchpad-window nil)
(defvar doom-tutorial--instructions-buffer-name "*tutorial instructions*")
(defvar doom-tutorial--cmd-log-buffer-name "*tutorial cmd-log*")

(defun doom-tutorial-setup-3-window ()
  (if (featurep! :ui workspaces)
      (progn
        (unless (+workspace-buffer-list)
          (+workspace-delete (+workspace-current-name)))
        (+workspace-switch doom-tutorial-workspace-name t))
    (setq doom-tutorial--old-windowconf (current-window-configuration))
    (delete-other-windows)
    (switch-to-buffer (doom-fallback-buffer)))
  ;; Setup do buffer
  (setq doom-tutorial--scratchpad-window (selected-window))
  (switch-to-buffer
   (get-buffer-create doom-tutorial--scratchpad-buffer-name))
  (with-silent-modifications
    (erase-buffer))
  (fundamental-mode)
  ;; Setup instruction buffer
  (split-window nil nil 'right)
  (select-window (next-window))
  (switch-to-buffer
   (get-buffer-create doom-tutorial--instructions-buffer-name))
  (with-silent-modifications
    (erase-buffer))
  (org-mode)
  (read-only-mode 1)
  (setq-local org-hide-emphasis-markers t)
  (when (bound-and-true-p spell-fu-mode)
    (spell-fu-mode -1))
  (when (bound-and-true-p flyspell-mode)
    (flyspell-mode -1))
  (when (bound-and-true-p writegood-mode)
    (writegood-mode -1))
  (when (bound-and-true-p solaire-mode)
    (solaire-mode -1))
  (display-line-numbers-mode -1)
  (org-indent-mode -1)
  (when (featurep 'org-superstar)
    (setq-local org-superstar-remove-leading-stars t)
    (org-superstar-restart))
  (setq-local header-line-format
              (propertize "Instructions" 'face '(bold org-document-title)))
  ;; Prevent cache errors
  (remove-hook 'eldoc-documentation-functions #'org-eldoc-documentation-function t)
  ;; Setup cmd log buffer
  (split-window nil (max window-min-height
                         (/ (window-height) 3))
                'above)
  (switch-to-buffer
   (get-buffer-create doom-tutorial--cmd-log-buffer-name))
  (with-silent-modifications
    (erase-buffer))
  (when (bound-and-true-p solaire-mode)
    (solaire-mode -1))
  (setq-local mode-line-format nil
              header-line-format
              (propertize "Command log" 'face '(bold org-document-title))
              scroll-margin 0)
  (select-window doom-tutorial--scratchpad-window))

(defun doom-tutorial-quit ()
  "Exit the current tutorial."
  (interactive)
  (when doom-tutorial--test-timer
    (cancel-timer doom-tutorial--test-timer))
  (doom-tutorial--save-progress)
  (cond
   ((and (featurep! :ui workspaces)
         (+workspace-exists-p doom-tutorial-workspace-name))
    (+workspace/delete doom-tutorial-workspace-name))
   (doom-tutorial--old-windowconf
    (set-window-configuration doom-tutorial--old-windowconf)
    (setq doom-tutorial--old-windowconf nil))))
