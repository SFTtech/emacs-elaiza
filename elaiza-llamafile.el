;;; elaiza-llamafile.el -*- lexical-binding: t; -*-
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
;; Implementation for interacting with a llamafile.
;;
;;; Code:

(require 'cl-lib)
(require 'json)
(require 'elaiza)
(require 'elaiza-request)

(defvar elaiza-llamafile-models)

(cl-defstruct (elaiza-llamafile (:include elaiza-backend (name "llamafile"))))

(cl-defmethod elaiza-request--encode (messages system-prompt (elaiza-backend
                                                              elaiza-llamafile))
  "Send MESSAGES to backend ELAIZA--BACKEND: Llamafile.

See https://platform.openai.com/docs/api-reference/chat
Add SYSTEM-PROMPT if non-nil.
See https://github.com/ggerganov/llama.cpp/blob/master/examples/server/README.md#change-system-prompt-on-runtime"
  (let ((headers '(("content-type" . "application/json")
                   ("accept-charset" . "utf-8")))
        (body (list (cons 'messages (if system-prompt (append
                                                       (list (list (cons 'role "system")
                                                                   (cons 'content system-prompt)))
                                                       messages)
                                      messages))
                    (cons 'stream  't)))
        (url "http://localhost:8080/v1/chat/completions"))
    (list url headers (encode-coding-string (json-encode body) 'utf-8))))

(cl-defmethod elaiza-request--parse-streamed-response (message-delta (elaiza-backend elaiza-llamafile))
  "Parse a partial stream response (MESSAGE-DELTA) from ELAIZA-BACKEND Llamafile."
  (when (and message-delta
             (string-match "\"delta\":{\"content\":\\(.*?\\)}" message-delta))
    (decode-coding-string (json-read-from-string (match-string 1 message-delta)) 'utf-8)))
