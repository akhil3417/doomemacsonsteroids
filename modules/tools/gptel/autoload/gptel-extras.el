;;; tools/gptel/autoload/gptel-extras.el -*- lexical-binding: t; -*-
;;;###if (modulep! +extras)

;; https://github.com/karthink/gptel/issues/171
(cl-defun +clean-up-gptel-refactored-code (beg end)
  "Clean up the code responses for refactored code in the current buffer.

The response is placed between BEG and END.  The current buffer is
guaranteed to be the response buffer."
  (when gptel-mode          ; Don't want this to happen in the dedicated buffer.
    (cl-return-from +clean-up-gptel-refactored-code))
  (when (and beg end)
    (save-excursion
      (let ((contents
             (replace-regexp-in-string
              "\n*``.*\n*" ""
              (buffer-substring-no-properties beg end))))
        (delete-region beg end)
        (goto-char beg)
        (insert contents))
      ;; Indent the code to match the buffer indentation if it's messed up.
      (indent-region beg end)
      (pulse-momentary-highlight-region beg end))))

(provide 'gptel-extras)
