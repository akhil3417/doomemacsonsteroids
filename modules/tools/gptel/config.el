;;; tools/gptel/config.el -*- lexical-binding: t; -*-

(require 'gptel)
(use-package! gptel
  :commands (gptel gptel-send)
  :hook ((gptel-mode . visual-fill-column-mode)
         (eshell-mode . +gptel-eshell-keys))
  :bind (("C-c S-<return>" . gptel-menu)
         ("C-c C-<return>" . gptel)
         ;; ("C-c C-c" . +gptel-send)
         ("C-h C-q" . gptel-quick) ;;over-binds help-quick-toggle
         :map gptel-mode-map
         ("C-c C-<return>" . gptel-menu)
         ("C-c C-c" . gptel-send)
         ("C-c C-x t" . gptel-set-topic))
  :init
  :config
  (defvar gptel-default-session "*Assistant*")
  (defalias '+gptel-easy-page
    (let* ((map (define-keymap
                  "SPC" 'scroll-up-command
                  "S-SPC" 'scroll-down-command
                  "RET" 'gptel-end-of-response)))
      (lambda ()
        (let ((scrolling (propertize  "SCRL" 'face '(:inherit highlight :weight bold))))
          (add-to-list 'mode-line-format scrolling)
          (set-transient-map
           map t
           (lambda () (setq mode-line-format
                            (delete scrolling mode-line-format))))))))
  (add-hook 'gptel-pre-response-hook '+gptel-easy-page)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (set-popup-rules! '(("^\\*Anyscale*" :slot 2 :side right :size 60 :select t :quit nil :ttl t)))

  (setf (alist-get 'org-mode gptel-response-prefix-alist)
        "*Response*: ")
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist)
        "*Prompt*: ")
  (setf (alist-get 'markdown-mode gptel-response-prefix-alist)
        "**Response4**: ")
  (setf (alist-get 'markdown-mode gptel-prompt-prefix-alist)
        "**Prompt**: ")

  (add-to-list
   'gptel-directives
   '(cliwhiz . "You are a command line helper. Generate command line commands that do what is requested, without any additional description or explanation. Generate ONLY the command, I will edit it myself before running.")
   :append)
  (add-to-list
   'gptel-directives
   '(emacser . "You are an Emacs maven. Reply only with the most appropriate built-in Emacs command for the task I specify. Do NOT generate any additional description or explanation.")
   :append)

  (defun +gptel-send (&optional arg)
    (interactive "P")
    (if (or gptel-mode (< (point) 2000) (use-region-p))
        (gptel-send arg)
      (if (y-or-n-p "Prompt has more than 2000 chars, send to LLM?")
          (gptel-send arg)
        (message "LLM: Request cancelled."))))
  (setq gptel-default-mode 'org-mode)
  (setf (alist-get "^\\*gptel-quick\\*" display-buffer-alist
                   nil nil #'equal)
        `((display-buffer-in-side-window)
          (side . bottom)
          (window-height . ,#'fit-window-to-buffer)))

  (defvar gptel-quick--history nil)

  (defun gptel-quick (prompt)
    (interactive (list (read-string "Ask LLM: " nil gptel-quick--history)))
    (when (string= prompt "") (user-error "A prompt is required."))
    (gptel-request
     prompt
     :callback
     (lambda (response info)
       (if (not response)
           (message "gptel-quick failed with message: %s" (plist-get info :status))
         (with-current-buffer (get-buffer-create "*gptel-quick*")
           (let ((inhibit-read-only t))
             (erase-buffer)
             (insert response))
           (special-mode)
           (display-buffer (current-buffer)))))))

  (defun +gptel-eshell-send (&optional arg)
    (interactive "P")
    (if (use-region-p)
        (gptel-send arg)
      (push-mark)
      (or (eshell-previous-prompt 0)
          (eshell-previous-prompt 1))
      (activate-mark)
      (gptel-send arg)
      (exchange-point-and-mark)
      (deactivate-mark)))
  (defun +gptel-eshell-keys ()
    (define-key eshell-mode-map (kbd "C-c <return>") #'+gptel-eshell-send)))

(when (featurep! +localapi)
  (gptel-make-gpt4all
   "GPT4All"                              ;Name of your choosing
   :protocol "http"
   :host "localhost:4891"                 ;Where it's running
   :models '("neuralbeagle14-7b.Q4_K_M.gguf")) ;Available models

                                        ; Define GPT model using gptel-make-openai
  (gptel-make-openai                    ;Not a typo, same API as OpenAI
   "llama-cpp"                          ; Any name
   :stream t                            ; Stream responses
   :protocol "http"
   :host "localhost:5000"               ; Llama.cpp server location, edited CMDflags.txt is text-gen-ui
   :key nil                             ; No key needed
   :models '("neuralbeagle14-7b.Q4_K_M.gguf"))

  (gptel-make-openai
   "llamafile"
   :stream t
   :protocol "http"
   :host "localhost:8080"               ;typically localhost:8080 for Llamafile - pass --server flag
   :key nil
   :models '("neuralbeagle14-7b.Q4_K_M.gguf"))

  (gptel-make-openai
   "kobold-cpp"
   :stream t
   :protocol "http"
   :host "localhost:5001"
   :key nil
   :models '("neuralbeagle14-7b.Q4_K_M.gguf"))

  )

(when (featurep! +gemini)
(gptel-make-gemini "Gemini"
  :key 'gemini-api-key
  :stream t))

;; Anyscale offers an OpenAI compatible API
(setq-default gptel-backend (gptel-make-openai "Anyscale")
              gptel-model   "mistralai/Mixtral-8x7B-Instruct-v0.1")
(setq-default
   gptel-backend (gptel-make-openai
	"Anyscale"
	:host "api.endpoints.anyscale.com"
	:header (lambda () `(("Authorization" . ,(concat "Bearer " (gptel--get-api-key)))))
	:key 'anyscale-api-key
        :stream t                            ; Stream responses
	:models '("mistralai/Mixtral-8x7B-Instruct-v0.1"
			 "meta-llama/Llama-2-70b-chat-hf"
			 "codellama/CodeLlama-34b-Instruct-hf"))
  gptel-model "mistralai/Mixtral-8x7B-Instruct-v0.1")
