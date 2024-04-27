;;; elaiza-claude.el --- Implementation for interacting with Anthropic's Claude -*- lexical-binding: t; -*-
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
;;  Implementation for interacting with Anthropic's Claude.
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'elaiza-request)
(require 'elaiza-backends)
(require 'elaiza-utils)

(defvar elaiza-claude-models
  (list
   '("Claude 3 Opus" . "claude-3-opus-20240229")  ; Most powerful model for highly complex tasks
   '("Claude 3 Sonnet" . "claude-3-sonnet-20240229")  ; Ideal balance of intelligence and speed for enterprise workloads
   '("Claude 3 Haiku" . "claude-3-haiku-20240307"))  ; Fastest and most compact model for near-instant responsiveness
  "List of available claude models.
See: https://docs.anthropic.com/claude/docs/models-overview")

(cl-defstruct (elaiza-claude (:include elaiza-backend (name "claude")))
  (key nil)
  (model (cdr (assoc "Claude 3 Haiku" elaiza-claude-models)))
  (max_tokens 10))

(defun elaiza-claude-get-api-key ()
  "Get Claude API key from auth-source, create if needed."
  (let* ((host "https://api.anthropic.com")
         (auth-source-creation-defaults
          '((description . "Claude API key")))
         (auth-source-creation-prompts
          '((secret . "Claude API key for %h: ")))
         (auth-info (nth 0 (auth-source-search
                            :max 1
                            :host host
                            :user "elaiza"
                            :create 't))))
    (if auth-info (auth-info-password auth-info)
      (error "Could not retrieve API key"))))

(defun elaiza-claude-delete-api-key ()
  "Delete Claude API key from auth-source."
  (interactive)
  (auth-source-delete
   :host "https://api.anthropic.com"
   :user "elaiza"))

(cl-defmethod elaiza-request--encode (messages system-prompt (elaiza-backend elaiza-claude))
  "Send MESSAGES to backend ELAIZA-BACKEND: Anthropic's Claude.

Add SYSTEM-PROMPT if non-nil.
See https://docs.anthropic.com/claude/reference/getting-started-with-the-api."
  (let ((headers `(("x-api-key" . ,(elaiza-claude-get-api-key))
                   ("anthropic-version" . "2023-06-01")
                   ("content-type" . "application/json")
                   ("accept-charset" . "utf-8")))
        (body `((:model . ,(elaiza-claude-model elaiza--backend))
                (:max_tokens . ,(elaiza-claude-max_tokens elaiza--backend))
                (:stream . t)
                (:messages . ,messages)))
        (url "https://api.anthropic.com/v1/messages"))
    (when elaiza-debug
      (message "Sending prompt to %s with %s max tokens"
               (elaiza-claude-model elaiza--backend)
               (elaiza-claude-max_tokens elaiza--backend)))
    (when system-prompt
      (push `(:system . ,system-prompt) body))
    (list url headers (encode-coding-string (json-encode body) 'utf-8))))

(cl-defmethod elaiza-request--parse-streamed-response (message-delta (elaiza-backend elaiza-claude))
  "Parse a partial stream response (MESSAGE-DELTA) from ELAIZA-BACKEND Claude."
  (when (and message-delta
             (string-match "\"text_delta\",\"text\":\\(.*?\\)}" message-delta))
    (decode-coding-string (json-read-from-string (match-string 1 message-delta)) 'utf-8)))

(provide 'elaiza-claude)
;;; elaiza-claude.el ends here
