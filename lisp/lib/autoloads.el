;;; lisp/lib/autoloads.el -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar doom-autoloads-excluded-packages ()
  "Which packages to exclude from Doom's autoloads files.

Use this for packages with problematic autoloads; e.g. they autoload too much or
hoist buggy forms into autoloads.")

(defvar doom-autoloads-excluded-files ()
  "List of regexps whose matching files won't be indexed for autoloads.")

(defvar doom-autoloads-cached-vars
  '(load-path
    auto-mode-alist
    interpreter-mode-alist
    magic-mode-alist
    magic-fallback-mode-alist
    Info-directory-list)
  "A list of variables to be cached in `doom-autoloads-file'.")

(defvar doom-autoloads-files ()
  "A list of additional files or file globs to scan for autoloads.")


;;
;;; Library

(defun doom-autoloads--write (file &rest forms)
  (make-directory (file-name-directory file) 'parents)
  (condition-case-unless-debug e
      (with-temp-file file
        (setq-local coding-system-for-write 'utf-8)
        (let ((standard-output (current-buffer))
              (print-quoted t)
              (print-level nil)
              (print-length nil))
          (insert ";; -*- lexical-binding: t; coding: utf-8; no-native-compile: t -*-\n"
                  ";; This file is autogenerated by 'doom sync', DO NOT EDIT IT!!\n")
          (dolist (form (delq nil forms))
            (mapc #'prin1 form))
          t))
    (error (delete-file file)
           (signal 'doom-autoload-error (list file e)))))

(defun doom-autoloads--compile-file (file)
  (condition-case-unless-debug e
      (let ((byte-compile-warnings (if init-file-debug byte-compile-warnings)))
        (and (byte-compile-file file)
             (load (byte-compile-dest-file file) nil t)))
    (error
     (delete-file (byte-compile-dest-file file))
     (signal 'doom-autoload-error (list file e)))))

(defun doom-autoloads--cleanup-form (form &optional expand)
  (let ((func (car-safe form)))
    (cond ((memq func '(provide custom-autoload register-definition-prefixes))
           nil)
          ((and (eq func 'add-to-list)
                (memq (doom-unquote (cadr form))
                      doom-autoloads-cached-vars))
           nil)
          ((not (eq func 'autoload))
           form)
          ((and expand (not (file-name-absolute-p (nth 2 form))))
           (defvar doom--autoloads-path-cache nil)
           (setf (nth 2 form)
                 (let ((path (nth 2 form)))
                   (or (cdr (assoc path doom--autoloads-path-cache))
                       (when-let* ((libpath (locate-library path))
                                   (libpath (file-name-sans-extension libpath))
                                   (libpath (abbreviate-file-name libpath)))
                         (push (cons path libpath) doom--autoloads-path-cache)
                         libpath)
                       path)))
           form)
          (form))))

(defun doom-autoloads--scan-autodefs (file buffer module &optional module-enabled-p)
  (with-temp-buffer
    (insert-file-contents file)
    (while (re-search-forward "^;;;###autodef *\\([^\n]+\\)?\n" nil t)
      (let* ((standard-output buffer)
             (form    (read (current-buffer)))
             (altform (match-string 1))
             (definer (car-safe form))
             (symbol  (doom-unquote (cadr form))))
        (cond ((and (not module-enabled-p) altform)
               (print (read altform)))
              ((memq definer '(defun defmacro cl-defun cl-defmacro))
               (print
                (if module-enabled-p
                    (make-autoload form (abbreviate-file-name file))
                  (seq-let (_ _ arglist &rest body) form
                    (if altform
                        (read altform)
                      (append
                       (list (pcase definer
                               (`defun 'defmacro)
                               (`cl-defun `cl-defmacro)
                               (_ type))
                             symbol arglist
                             (format "THIS FUNCTION DOES NOTHING BECAUSE %s IS DISABLED\n\n%s"
                                     module (if (stringp (car body))
                                                (pop body)
                                              "No documentation.")))
                       (cl-loop for arg in arglist
                                if (symbolp arg)
                                if (not (keywordp arg))
                                if (not (memq arg cl--lambda-list-keywords))
                                collect arg into syms
                                else if (listp arg)
                                collect (car arg) into syms
                                finally return (if syms `((ignore ,@syms)))))))))
               (print `(put ',symbol 'doom-module ',module)))
              ((eq definer 'defalias)
               (seq-let (_ _ target docstring) form
                 (unless module-enabled-p
                   (setq target #'ignore
                         docstring
                         (format "THIS FUNCTION DOES NOTHING BECAUSE %s IS DISABLED\n\n%s"
                                 module docstring)))
                 (print `(put ',symbol 'doom-module ',module))
                 (print `(defalias ',symbol #',(doom-unquote target) ,docstring))))
              (module-enabled-p (print form)))))))

(defvar autoload-timestamps)
(defvar generated-autoload-load-name)
(defun doom-autoloads--scan-file (file)
  (let* (;; Prevent `autoload-find-file' from firing file hooks, e.g. adding
         ;; to recentf.
         find-file-hook
         write-file-functions
         ;; Prevent a possible source of crashes when there's a syntax error in
         ;; the autoloads file.
         debug-on-error
         ;; Non-nil interferes with autoload generation in Emacs < 29. See
         ;; radian-software/straight.el#904.
         (left-margin 0)
         ;; The following bindings are in `package-generate-autoloads'.
         ;; Presumably for a good reason, so I just copied them.
         (backup-inhibited t)
         (version-control 'never)
         case-fold-search    ; reduce magic
         autoload-timestamps ; reduce noise in generated files
         autoload-compute-prefixes
         ;; So `autoload-generate-file-autoloads' knows where to write it
         (target-buffer (current-buffer))
         (module (doom-module-from-path file))
         (generated-autoload-load-name (abbreviate-file-name (file-name-sans-extension file)))
         (module-enabled-p (and (doom-module-p (car module) (cdr module))
                                (doom-file-cookie-p file "if" t))))
    (save-excursion
      (when module-enabled-p
        (quiet! (autoload-generate-file-autoloads file target-buffer)))
      (doom-autoloads--scan-autodefs
       file target-buffer module module-enabled-p))))

(defun doom-autoloads--scan (files &optional exclude literal)
  "Scan and return all autoloaded forms in FILES.

Autoloads will be generated from autoload cookies in FILES (except those that
match one of the regexps in EXCLUDE -- a list of strings). If LITERAL is
non-nil, treat FILES as pre-generated autoload files instead."
  (require 'autoload)
  (let (autoloads)
    (dolist (file files (nreverse (delq nil autoloads)))
      (when (and (not (seq-find (doom-rpartial #'string-match-p file) exclude))
                 (file-readable-p file))
        (doom-log "loaddefs:scan: %s" file)
        (setq file (file-truename file))
        (with-temp-buffer
          (if literal
              (insert-file-contents file)
            (doom-autoloads--scan-file file))
          (save-excursion
            (while (re-search-forward "\\_<load-file-name\\_>" nil t)
              ;; `load-file-name' is meaningless in a concatenated
              ;; mega-autoloads file, but also essential in isolation, so we
              ;; replace references to it with the file they came from.
              (let ((ppss (save-excursion (syntax-ppss))))
                (or (nth 3 ppss)
                    (nth 4 ppss)
                    (replace-match (prin1-to-string (abbreviate-file-name file)) t t)))))
          (let ((load-file-name file)
                (load-path
                 (append (list doom-user-dir)
                         doom-module-load-path
                         load-path)))
            (condition-case _
                (while t
                  (push (doom-autoloads--cleanup-form (read (current-buffer))
                                                      (not literal))
                        autoloads))
              (end-of-file))))))))

(provide 'doom-lib '(autoloads))
;;; autoloads.el end here
