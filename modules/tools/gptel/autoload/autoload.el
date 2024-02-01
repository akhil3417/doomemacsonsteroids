;;; tools/gptel/autoload/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(defcustom gemini-api-key #'gptel-api-key-from-auth-source
  "An API key (string) for the Gemini backend.

Can also be a function of no arguments that returns an API
key (more secure) for the active backend."
  :group 'gptel
  :type '(choice
          (string :tag "API key")
          (function :tag "Function that returns the API key")))

;;;###autoload
(defcustom anyscale-api-key #'gptel-api-key-from-auth-source
  "An API key (string) for the Anyscale backend.

Can also be a function of no arguments that returns an API
key (more secure) for the active backend."
  :group 'gptel
  :type '(choice
          (string :tag "API key")
          (function :tag "Function that returns the API key")))
