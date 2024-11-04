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

(cl-defstruct (elaiza-anthropic (:include elaiza-backend name))
  "For a list of available models, see https://docs.anthropic.com/claude/docs/models-overview."
  key model max_tokens)

(cl-defstruct (elaiza-claude-opus (:include elaiza-anthropic (name "Claude 3 Opus")
                                            (key nil)
                                            (model "claude-3-opus-latest")
                                            (max_tokens 4096)))
 "Powerful model for highly complex tasks.")

(cl-defstruct (elaiza-claude-sonnet-3-5 (:include elaiza-anthropic (name "Claude 3.5 Sonnet")
                                            (key nil)
                                            (model "claude-3-5-sonnet-latest")
                                            (max_tokens 8192)))
  "Our most intelligent model.")

(cl-defstruct (elaiza-claude-sonnet (:include elaiza-anthropic (name "Claude 3 Sonnet")
                                            (key nil)
                                            (model "claude-3-sonnet-20240229")
                                            (max_tokens 4096)))
  "Balance of intelligence and speed.")

(cl-defstruct (elaiza-claude-haiku-3-5 (:include elaiza-anthropic (name "Claude 3.5 Haiku")
                                            (key nil)
                                            (model "claude-3-5-haiku-20241022")
                                            (max_tokens 8192)))
  "Our fastest model.")

(cl-defstruct (elaiza-claude-haiku (:include elaiza-anthropic (name "Claude 3 Haiku")
                                            (key nil)
                                            (model "claude-3-haiku-20240307")
                                            (max_tokens 4096)))
  "Fastest and most compact model for near-instant responsivenes.")

(defun elaiza-claude-get-api-key ()
  "Get Claude API key from auth-source, create if needed."
  (let* ((auth-source-creation-defaults
          '((description . "Claude API key")))
         (auth-source-creation-prompts
          '((secret . "Claude API key for %h: ")))
         (auth-info (nth 0 (auth-source-search
                            :max 1
                            :host "api.anthropic.com"
                            :user "elaiza"
                            :create t))))
    (if auth-info (auth-info-password auth-info)
      (error "Could not retrieve API key\nSave machine api.anthropic.com port https login elaiza password <your-api-key> in ~/.authinfo.gpg"))))

(cl-defmethod elaiza-request--encode (messages system-prompt (backend elaiza-anthropic))
  "Send MESSAGES to BACKEND: Anthropic's Claude.
Add SYSTEM-PROMPT if non-nil.
See https://docs.anthropic.com/claude/reference/getting-started-with-the-api."
  (let ((headers `(("x-api-key" . ,(encode-coding-string
                                    (elaiza-claude-get-api-key)
                                    'utf-8))
                   ("anthropic-version" . "2023-06-01")
                   ("content-type" . "application/json")
                   ("accept-charset" . "utf-8")))
        (body (list (cons 'model (elaiza-anthropic-model backend))
                    (cons 'max_tokens (elaiza-anthropic-max_tokens backend))
                    (cons 'stream 't)
                    (cons 'messages messages)))
        (url "https://api.anthropic.com/v1/messages"))
    (when elaiza-debug
      (message "Sending prompt to %s with %s max tokens"
               (elaiza-anthropic-model backend)
               (elaiza-anthropic-max_tokens backend)))
    (when system-prompt
      (push (cons 'system system-prompt) body))
    (list url headers (encode-coding-string (json-encode body) 'utf-8))))

(cl-defmethod elaiza-request--parse-streamed-response (message-delta (_ elaiza-anthropic) _)
  "Parse a partial stream response (MESSAGE-DELTA) from ELAIZA-BACKEND Claude."
  (when (and message-delta
             (string-match "\"text_delta\",\"text\":\\(.*?\\)}" message-delta))
    (decode-coding-string (json-read-from-string (match-string 1 message-delta)) 'utf-8)))

(provide 'elaiza-claude)
;;; elaiza-claude.el ends here
