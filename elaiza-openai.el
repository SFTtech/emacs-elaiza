;;; elaiza-openai.el --- Implementation for interacting with OpenAI's ChatGPT -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Alessandro Wollek
;;
;; Author: Alessandro Wollek <contact@wollek.ai>
;; Homepage: https://github.com/SFTtech/emacs-elaiza
;; SPDX-License-Identifier: GPL-3.0-only
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Implementation for interacting with OpenAI's ChatGPT.
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'elaiza-backends)
(require 'elaiza-request)

(cl-defstruct (elaiza-openai (:include elaiza-backend name))
  "For a list of available models, see https://platform.openai.com/docs/models."
  key model max_tokens)

(cl-defstruct (elaiza-gpt-4-turbo (:include elaiza-openai (name "GPT-4 Turbo")
                                            (key nil)
                                            (model "gpt-4-turbo")
                                            (max_tokens 'nil))) ; 128000
  "The latest GPT-4 Turbo model. GPT-4 Turbo is more powerful than GPT-4 and offered at a lower price.")

(cl-defstruct (elaiza-gpt-4 (:include elaiza-openai (name "GPT-4")
                                      (key nil)
                                      (model "gpt-4")
                                      (max_tokens 'nil))) ; 8192
  "The latest GPT-4 model.")

(cl-defstruct (elaiza-gpt-4o (:include elaiza-openai (name "GPT-4o")
                                      (key nil)
                                      (model "gpt-4o")
                                      (max_tokens 'nil))) ; 12800
  "The fastest and most affordable flagship model")

(cl-defstruct (elaiza-gpt-3.5-turbo (:include elaiza-openai
                                              (name "GPT-3.5 Turbo")
                                              (key nil)
                                              (model "gpt-3.5-turbo")
                                              (max_tokens 'nil))) ; 16385
  "A fast, inexpensive model for simple tasks.")

(defun elaiza-openai-get-api-key ()
  "Get OpenAI API key from auth-source, create if needed."
  (let* ((auth-source-creation-defaults
          '((description . "OpenAI API key")))
         (auth-source-creation-prompts
          '((secret . "OpenAI API key for %h: ")))
         (auth-info (nth 0 (auth-source-search
                            :max 1
                            :host "api.openai.com"
                            :user "elaiza"))))
    (if auth-info (auth-info-password auth-info)
      (error "Could not retrieve API key\nSave machine api.openai.com port https login elaiza password <your-api-key> in ~/.authinfo.gpg"))))

(cl-defmethod elaiza-request--encode (messages system-prompt (backend elaiza-openai))
  "Send MESSAGES to ELAIZA-BACKEND OpenAI.

See https://platform.openai.com/docs/api-reference/chat
Add SYSTEM-PROMPT if non-nil.
See https://github.com/ggerganov/llama.cpp/blob/master/examples/server/README.md#change-system-prompt-on-runtime"
  (let ((headers `(("authorization" . ,(concat "Bearer " (elaiza-openai-get-api-key)))
                   ("content-type" . "application/json")
                   ("accept-charset" . "utf-8")))
        (body (list (cons 'messages (if system-prompt (append
                                                       (list (list (cons 'role "system")
                                                                   (cons 'content system-prompt)))
                                                       messages)
                                      messages))
                    (cons 'stream  't)
                    (cons 'model (elaiza-openai-model backend))
                    (cons 'max_tokens (elaiza-openai-max_tokens backend))))
        (url "https://api.openai.com/v1/chat/completions"))
    (list url headers (encode-coding-string (json-encode body) 'utf-8))))

(cl-defmethod elaiza-request--parse-streamed-response (message-delta (_ elaiza-openai))
  "Parse a partial stream response (MESSAGE-DELTA) from ELAIZA-BACKEND Chatgpt."
  (when (and message-delta
             (string-match ":{\"content\":\\(.*?\\)},\"logprobs\"" message-delta))
    (decode-coding-string (json-read-from-string (match-string 1 message-delta)) 'utf-8)))

(provide 'elaiza-openai)
;;; elaiza-openai.el ends here
