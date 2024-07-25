;;; ollama-copilot.el --- An ollama Emacs buddy -*- lexical-binding: t; -*-

;;; Commentary:


;; Configs:
;;
;; (setq ollama-copilot-host "http://temporary-host:6789")
;; (setq ollama-copilot-context-lines 90)
;; (setq ollama-copilot-model "granite-code:34b")
;;
;; This file provides TBD

;;; Code:

(require 'json)
(require 'url)

(defgroup ollama-copilot nil
  "Large language model code completion."
  :prefix "ollama-copilot-"
  :group 'editing)

(defcustom ollama-copilot-context-lines 50
  "Number of lines before the current position to include in the context."
  :type 'integer
  :group 'ollama-copilot)

(defcustom ollama-copilot-model "llama3.1"
  "Host of ollama server."
  :type 'string
  :group 'ollama-copilot)

(defcustom ollama-copilot-host "http://localhost:11434"
  "Host of ollama server."
  :type 'string
  :group 'ollama-copilot)

;; (customize-set-variable 'ollama-copilot-llm-code-complete-prompt (eval (car (get 'ollama-copilot-llm-code-complete-prompt 'standard-value))))
(defcustom ollama-copilot-llm-code-complete-prompt
  "You are an Emacs code generator.
I need assistance generating the code that complete the following code in the buffer %s.
Here is the code I have so far:

%s

Only respond with the code to complete, anythin else.
Writing comments, test code, or English explanations is forbidden.
"
  "Default prompt to use when calling the LLM for code completion."
  :type 'string
  :group 'ollama-copilot)

(defcustom ollama-copilot-llm-refactor-prompt
  "You are an Emacs code generator.
I need assistance refactoring the following code in in the buffer %s.

%s

Apply the best practices to improve it. Use the SOLID principles
"
  "Default prompt to use when calling the LLM for code completion."
  :type 'string
  :group 'ollama-copilot)

(defun ollama-copilot-complete-code ()
  "Generate code completion using the Ollama LLM."
  (interactive)
  (let ((prompt (_get-code-complete-prompt))
        (generated-text))
    ;; (message "generated prompt: %s" prompt)
    (setq generated-text (_call-llm prompt))
    ;; (message "generated text: %s" generated-text)
    (when generated-text
      (save-excursion
        (insert generated-text)))))

(defun ollama-copilot-refactor ()
  "Generate code using LLM and prompt user to replace the selected code."
  (interactive)
  (let* ((original-code (buffer-substring (region-beginning) (region-end)))
         (prompt (format ollama-copilot-llm-refactor-prompt (buffer-name) original-code))
         (generated-code (_call-llm prompt)))
    ;; Display the generated code in a temporary buffer
    (with-temp-buffer-window "*Generated Code*" nil nil (princ generated-code))
    (if (yes-or-no-p "Do you want to replace the selected code with the generated code?")
        (progn
          (delete-region (region-beginning) (region-end))
          (insert generated-code)
          (message "Code replaced."))
      (message "Operation cancelled."))))

(defun ollama-copilot-list-models ()
  "List available models from the Ollama LLM API."
  (interactive)
  (let* ((url-request-method "GET")
 (url (format "%s/api/tags" ollama-copilot-host)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (let* ((response-string (decode-coding-string (buffer-substring-no-properties (point) (point-max)) 'utf-8))
             (response (json-read-from-string response-string))
             (models (append (cdr (assoc 'models response)) nil)) ;; Convert vector to list
             (model-names (mapcar (lambda (model) (cdr (assoc 'name model))) models))
             (model-names-str (mapconcat 'identity model-names ", ")))
        (kill-buffer (current-buffer))
        (message "Local models: %s" model-names-str)))))

(global-set-key (kbd "C-c o") 'ollama-copilot-complete-code)
(global-set-key (kbd "C-c r") 'ollama-copilot-refactor)
(global-set-key (kbd "C-c l") 'ollama-copilot-list-models)

(provide 'ollama-copilot)

(defun _get-code-complete-prompt ()
  "Generate the prompt to complete code."
  (interactive)
  (let* ((start-pos (save-excursion
              (forward-line (- ollama-copilot-context-lines))
                      (point)))
         (surrounding-text (buffer-substring-no-properties start-pos (point))))
    (format ollama-copilot-llm-code-complete-prompt (buffer-name) surrounding-text)))

(defun _strip-markdown (text)
  "Strip markdown formatting from the given TEXT."
  (message text)
  (replace-regexp-in-string "```\\|\\`\\|\\'" "" text))

(defun _call-llm (prompt)
  "Call the Ollama LLM API with the given PROMPT."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode `(("model" . ,ollama-copilot-model)
                                                                ("stream" . :json-false)
                                                        ("prompt" . ,prompt))) 'utf-8))
         (url (format "%s/api/generate" ollama-copilot-host)))
    ;; (message url-request-data)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (let* ((response (json-read))
             ;; (generated-text (_strip-markdown (assoc-default 'response response))))
             (generated-text (assoc-default 'response response)))
        (message "API Response: %s. Model %s" response ollama-copilot-model)
        (kill-buffer (current-buffer))
        generated-text))))


;;; ollama-copilot.el ends here
