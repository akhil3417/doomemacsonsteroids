;;; tools/gptel/autoload/gptel-extra-prompts.el -*- lexical-binding: t; -*-
;;;###if (modulep! +aipihkal)

;; taken from https://github.com/gregoryg/AIPIHKAL

;;;###autoload
(defun +gptel-build-directives (promptdir)
  "Build `gptel-directives' from Markdown files in PROMPTDIR."
  (let* ((prompt-files (directory-files promptdir t "md$")))
    (mapcar (lambda (prompt-file)
              ;; (list (intern (f-base prompt-file)) "filler1" "filler2")
              (with-temp-buffer
                (insert-file-contents prompt-file)
                (let ((prompt-description "NO DESCRIPTION")
                      (prompt-text nil))
                  ;; nab the description - single-line descriptions only!
                  (goto-char (point-min))
                  (when (re-search-forward "#\\+description: \\(.*?\\) *--> *$" nil t)
                    (setq prompt-description (match-string 1)))
                  ;; remove all comments
                  (delete-matching-lines "^ *<!--" (point-min) (point-max))
                  (delete-matching-lines "^$" (point-min) (+ 1 (point-min))) ; remove first blank line if exists
                  (goto-char (point-min)) ;; not necessary, point is in the midst of comments to start

                  ;; return the megillah
                  (list
                   (intern (f-base prompt-file)) ; gptel-directives key
                   prompt-description
                   (buffer-substring-no-properties (point-min) (point-max)) ))))
            prompt-files)))

;;;###autoload
(defun +gptel--annotate-directives (s)
  "Make the directives selection look fancy."
  (let* ((item (assoc (intern s) minibuffer-completion-table))
         (desc (s-truncate 40 (nth 1 item)))
         (prompt (s-truncate 80 (s-replace "\n" "\\n" (nth 2 item)))))
    (when item (concat
                (string-pad "" (- 40 (string-width s)))
                desc
                (string-pad "" (- 55 (string-width desc)))
                prompt
                ))))

;;;###autoload
(defun +gptel-select-system-prompt (&optional directive-key)
;; (defun gptel-system-prompt (&optional directive-key)
  "Set system message in local gptel buffer to directive/prompt indicated by DIRECTIVE-KEY."
  (interactive)
  (let* ((marginalia-align-offset 80)
         (completion-extra-properties '(:annotation-function +gptel--annotate-directives))
         (directive-key (or directive-key
                            (intern
                             (completing-read
                              ;; "New directive: "
                              (format "Current prompt %s: "
                                      (truncate-string-to-width gptel--system-message 90 nil nil (truncate-string-ellipsis) ))
                              gptel-directives
                              nil ;; predicate/filter
                              nil ;; do not require a match - allow custom prompt
                              nil ;; no initial input
                              nil ;; no history specified
                              "default" ;; default value if return is nil
                              )))))
    (setq-local gptel--system-message (nth 2 (assoc directive-key gptel-directives)))))

(setq gptel-directives (+gptel-build-directives(concat doom-local-dir "straight/repos/AIPIHKAL/system-prompts/")))

(provide 'gptel-extras-prompts)
