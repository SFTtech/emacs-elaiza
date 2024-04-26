;;; elaiza-request.el --- API interface for ELAIZA -*- lexical-binding: t; -*-
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
;;  API interface for ELAIZA
;;
;;; Code:

(require 'cl-lib)
(require 'url-http)
(require 'elaiza-utils)

(cl-defgeneric elaiza-request--encode (messages system-prompt backend)
  "Encode MESSAGES and SYSTEM-PROMPT for BACKEND request.")

(defun elaiza-request (prompt system-prompt on-success on-streamed-response backend)
  "Send PROMPT and SYSTEM-PROMPT to BACKEND.

After a successful request ON-SUCCESS is called.
For resolving the streamed response ON-STREAMED-RESPONSE is used."
  (let ((url-request-method "POST"))
    (cl-multiple-value-bind (url url-request-extra-headers url-request-data)
        (elaiza-request--encode prompt system-prompt backend)

      (when elaiza-debug
        (with-current-buffer (get-buffer-create "*elaiza-log*")
          (goto-char (point-max))
          (insert "\n\n" url-request-data))

        (with-current-buffer (url-retrieve url on-success 'nil)
          (add-hook 'after-change-functions (elaiza-request--after-change-function on-streamed-response backend) 'nil 't))))))

(defun elaiza-request--after-change-function (on-streamed-response backend)
  "Function is intended to be used as `after-change-functions' hook.

ON-STREAMED-RESPONSE: Callback with the response string as argument.
Parsing is BACKEND specific."
  (lambda (beg end _)
    (elaiza-request--process-streamed-response beg end on-streamed-response backend)))

(defun elaiza-request--process-streamed-response (beg end on-streamed-response backend)
  "Process streamed response from BEG to END via BACKEND.
Parsed response is passed to ON-STREAMED-RESPONSE callback.

Check response line-by-line for text delta.
BEG and END refer to `after-change-functions' hook's arguments."
  (goto-char beg)
  (while (< (point) end)
    (when-let ((message-delta (buffer-substring-no-properties (point) (progn (end-of-line) (point))))
               (result (elaiza-request--parse-streamed-response message-delta backend)))
      ;; Mark LLM text for a continued conversation.
      (add-text-properties 0 (length result) '(role "assistant") result)
      (funcall on-streamed-response result))
    (forward-line 1)))

(cl-defgeneric elaiza-request--parse-streamed-response (message-delta backend)
  "Parse MESSAGE-DELTA of the request buffer from streamed response with BACKEND.")




(provide 'elaiza-request)
;;; elaiza-request.el ends here
