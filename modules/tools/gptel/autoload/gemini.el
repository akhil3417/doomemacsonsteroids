;;; tools/gptel/autoload/gemini.el -*- lexical-binding: t; -*-

;;;###autoload
(require 'gptel-gemini)
;; (load-file (concat doom-local-dir "straight/repos/gptel/gptel-gemini.el"))

;;taken from https://github.com/benthamite/dotfiles/blob/master/emacs/extras/gptel-extras.el

(defgroup gptel-extras ()
  "Extensions for `gptel'."
  :group 'gptel-extras)

;;;; Variables

;;;###autoload
(defvar gptel-extras-gemini-pro-backend-plist
  `(:key 'gemini-api-key
    :stream t)
  "Parameters for creating a Gemini Pro backend.")

;;;###autoload
(defvar gptel-extras-gemini-pro-backend
  (apply #'gptel-make-gemini "Gemini" gptel-extras-gemini-pro-backend-plist)
  "Backend for `gptel' when using the Gemini Pro model.")

;;;###autoload
(defvar gptel-extras-backends
  `(("gemini-pro" . ,gptel-extras-gemini-pro-backend))
  "List of backends for `gptel'.")

;; (defvar gptel-extras-backends
;;   `(("gpt-4" . ,gptel--openai)
;;     ("gemini-pro" . ,gptel-extras-gemini-pro-backend))
;;   "List of backends for `gptel'.")
;;;; Functions

;;;###autoload
(defun gptel-extras-model-config (model)
  "Configure `gptel' for MODEL."
  (interactive (list (completing-read "Model: " gptel-extras-backends nil t)))
  (setq gptel-model model
	gptel-backend (alist-get model gptel-extras-backends nil nil #'string=)))

;; (when (featurep! +gemini)
;; (setq-default gptel-model "gemini-pro" ; choose gemini-pro as def
;;               gptel-backend (apply #'gptel-make-gemini "Gemini" gptel-extras-gemini-pro-backend-plist)))
