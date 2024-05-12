;;; elaiza-ollama.el --- Implementation for interacting with a Ollama -*- lexical-binding: t; -*-
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
;; Implementation for interacting with a Ollama.
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'elaiza-backends)
(require 'elaiza-request)

(defvar elaiza-ollama-models)

(cl-defstruct (elaiza-ollama (:include elaiza-backend (name "Ollama")))
  "Ollama model."
  (port "11434") ; default port
  (model "llama2")) ; default model

(cl-defmethod elaiza-request--encode (messages system-prompt (backend elaiza-ollama))
  "Send MESSAGES to BACKEND: Ollama.

See https://github.com/ollama/ollama/blob/main/docs/api.md
Add SYSTEM-PROMPT if non-nil."
  (let ((headers '(("content-type" . "application/json")
                   ("accept-charset" . "utf-8")))
        (body (list (cons 'messages (if system-prompt (append
                                                       (list (list (cons 'role "system")
                                                                   (cons 'content system-prompt)))
                                                       messages)
                                      messages))
                    (cons 'stream  't)
                    (cons 'model (elaiza-ollama-model backend))))
        (url (concat "http://localhost:" (elaiza-ollama-port backend) "/api/chat")))
    (list url headers (encode-coding-string (json-encode body) 'utf-8))))

(cl-defmethod elaiza-request--parse-streamed-response (message-delta (_ elaiza-ollama))
  "Parse a partial stream response (MESSAGE-DELTA) from ELAIZA-BACKEND Ollama."
  (when (and message-delta
             (string-match "\"content\":\\(.*?\\)}" message-delta))
    (decode-coding-string (json-read-from-string (match-string 1 message-delta)) 'utf-8)))

(provide 'elaiza-ollama)
;;; elaiza-ollama.el ends here
