;;; ollama-copilot.el --- An ollama Emacs buddy -*- lexical-binding: t; -*-

;;; Commentary:

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

(defcustom ollama-copilot-ollama-host "localhost"
  "Host of ollama server."
  :type 'string
  :group 'ollama-copilot)

(defcustom ollama-copilot-llm-code-complete-prompt
  "You are an Emacs code generator working in the buffer %s.
Given the following context
\`\`\`
%s
\`\`\`

Generate the code to complete the current function.
Writing comments, test code, or English explanations is forbidden.
"
  "Default prompt to use when calling the LLM for code completion."
  :type 'string
  :group 'ollama-copilot)

(defun ollama-copilot-get-code-complete-prompt ()
  "Generate the prompt to complete code."
  (interactive)
  (let* ((start-pos (save-excursion
                      (forward-line (- ollama-copilot-context-lines))
                      (point)))
         (surrounding-text (buffer-substring-no-properties start-pos (point))))
    (format ollama-copilot-llm-code-complete-prompt (buffer-name) surrounding-text)))

(defun ollama-copilot-strip-markdown (text)
  "Strip markdown formatting from the given TEXT."
  (replace-regexp-in-string "```\\|\\`\\|\\'" "" text))

(defun ollama-copilot-call-llm (prompt)
  "Call the Ollama LLM API with the given PROMPT."
  (let* ((url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode `(("model" . "llama3")
                                                                ("stream" . :json-false)
                                                                ("prompt" . ,prompt))) 'utf-8))
         (url (format "http://%s:11434/api/generate" ollama-copilot-ollama-host)))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char url-http-end-of-headers)
      (let* ((response (json-read))
             (generated-text (ollama-copilot-strip-markdown (assoc-default 'response response))))
        (kill-buffer (current-buffer))
        generated-text))))

(defun ollama-copilot-complete-code ()
  "Generate code completion using the Ollama LLM."
  (interactive)
  (let ((prompt (ollama-copilot-get-code-complete-prompt))
        (generated-text))
    (setq generated-text (ollama-copilot-call-llm prompt))
    (when generated-text
      (save-excursion
        (insert generated-text)))))

(global-set-key (kbd "C-c o") 'ollama-copilot-complete-code)

(provide 'ollama-copilot)

;;; ollama-copilot.el ends here
