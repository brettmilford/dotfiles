;;; config/assistant/config.el -*- lexical-binding: t; -*-

(defun +assistant-eval-config ()
(use-package! gptel
  :init
  (setq evil-collection-gptel-want-ret-to-send nil
        evil-collection-gptel-want-shift-ret-menu nil)
  :config
  (setq gptel-log-level 'debug
        gptel-default-mode 'org-mode)

  (defun +a/gptel-popup ()
    "Launch gptel in a new frame."
    (interactive)
    (let ((new-frame (make-frame '((width . 80)(height . 20))))
          (buffer-name (generate-new-buffer-name "*GPTel*")))
      (select-frame new-frame)
      (delete-other-windows)
      (gptel buffer-name nil nil nil)
      (switch-to-buffer buffer-name)
      (when (bound-and-true-p evil-mode)
        (evil-insert-state))))

  (map! :g "M-SPC" #'+a/gptel-popup)

  (defun +a/gptel-toggle-branching ()
    "Toggle settings for org-branching-context"
    (interactive)
    (cond
     (gptel-org-branching-context
      (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "*** ")
      (setf (alist-get 'org-mode gptel-response-prefix-alist) "")
      (setq gptel-org-convert-response nil
            gptel-org-branching-context nil))
     ((not gptel-org-branching-context)
      (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@user\n")
      (setf (alist-get 'org-mode gptel-response-prefix-alist) "@assistant\n")
      (setq gptel-org-convert-response nil
            gptel-org-branching-context 't))))

  (let ((directory "~/org/src/llm_system_prompts"))
    (dolist (file (directory-files directory nil nil))
      (unless (file-directory-p file)
        (with-temp-buffer
          (insert-file-contents (concat directory "/" file))
          (let ((content (buffer-string))
                (filename (file-name-sans-extension file)))
            (push (cons (intern filename) content) gptel-directives))))))

  (setq gptel-model 'llm
        gptel-max-tokens nil
        gptel-temperature nil ;; should get temp from model config
        gptel-expert-commands nil ;; note if gptel-temperature is nil this breaks the transient
        gptel-include-reasoning 'ignore)

  ;; Backends

  (setq gptel-backend
    (gptel-make-openai "llama-cpp"
     :stream t
     :protocol "http"
     :host "localhost:8080"
     :models '(llm)))

   (defun +a/gptel-ollama-get-models (backend-name)
     "Format Ollama models for gptel from backend named BACKEND-NAME."
     (interactive)
     (let* ((backend (alist-get backend-name gptel--known-backends nil nil #'equal))
            (backend-url (when backend (concat (gptel-backend-protocol backend) "://"
                                               (gptel-backend-host backend))))
            (url (when backend-url (concat backend-url "/api/tags")))
            (result '())
            ;; Model families with descriptions
            (model-descriptions
             '(("llama" . "Open source large language model")
               ("mistral" . "Efficient and powerful open language model")
               ("phi" . "Microsoft's lightweight and efficient model")
               ("codellama" . "Specialized for code generation and completion")
               ("vicuna" . "Fine-tuned assistant based on LLaMA")
               ("orca" . "Research-focused language model")
               ("falcon" . "Open language model with balanced performance")
               ("mpt" . "MosaicML's efficient transformer architecture")
               ("yi" . "Open foundation language model from 01.AI")
               ("stablelm" . "Stability AI's language model")
               ("qwen" . "Alibaba's versatile language model")
               ("openchat" . "Open conversational language model")
               ("gemma" . "Google's lightweight open model")
               ("nous-hermes" . "Instruction-tuned conversational assistant")
               ("dolphin" . "Fine-tuned conversational model")
               ("wizard" . "Instruction-following language model"))))

       (unless url
         (error "Backend %s not found or has no URL" backend-name))

       (condition-case err
           (let* ((response (with-temp-buffer
                              (url-insert-file-contents url)
                              (json-read)))
                  (models (append (alist-get 'models response) nil)))

             (dolist (model models)
               (let* ((name (alist-get 'name model))
                      (model-family (car (split-string name ":")))
                      (digest (alist-get 'digest model))
                      (size (/ (or (alist-get 'size model) 0) (* 1024 1024 1024.0))) ; Convert to GB
                      (modified (alist-get 'modified_at model))
                      (quantization (when (string-match "\\(q[0-9]+_[0-9]+\\)" name)
                                      (match-string 1 name)))
                      (description
                       (or (cdr (assoc model-family model-descriptions))
                           (format "Ollama model (%.1fGB)" size)))
                      (context-window
                       (cond
                        ((string-match-p "70b" name) 4)      ; 4k context
                        ((string-match-p "13b\\|14b" name) 8) ; 8k context
                        ((string-match-p "7b" name) 8)       ; 8k context
                        (t 4)))                              ; default 4k
                      (model-entry
                       `(,(intern name)
                         :description ,(concat description
                                               (if quantization
                                                   (format " (%s)" quantization)
                                                 ""))
                         :context-window ,context-window
                         :input-cost 0.0 ; Free/local models
                         :output-cost 0.0
                         :capabilities ,(cond
                                         ((string-match-p "codellama\\|starcoder\\|wizard-coder" name)
                                          '(code))
                                         ((string-match-p "vision" name)
                                          '(media))
                                         (t nil)))))

                 (push model-entry result))))

         (error (message "Error fetching Ollama models: %s" (error-message-string err))))

       (nreverse result)
       (setf (gptel-backend-models backend) result)))

   (gptel-make-ollama "ollama"
     :stream t)
   ;; NOTE: ollama endpoint must be available
   ;; (+a/gptel-ollama-get-models "ollama")

   (gptel-make-gh-copilot "Copilot")

   (gptel-make-anthropic "Claude" :stream t :key gptel-api-key))

  ;; (setf (gptel-get-backend "ChatGPT") nil)
  (message "config/assistant/config.el was evaluated"))
