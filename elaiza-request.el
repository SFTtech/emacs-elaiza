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
(require 'mm-decode)
(require 'elaiza-backends)
(require 'elaiza-utils)

(defvar elaiza-request--buffer nil
  "Buffer-local variable to keep track of the current connection.
Used for interrupting connections.")

(cl-defgeneric elaiza-request--encode (messages system-prompt backend)
  "Encode MESSAGES and SYSTEM-PROMPT for BACKEND request.")

(defun elaiza--request (prompt system-prompt on-success on-streamed-response backend &optional elaiza-buffer on-end)
  "Use `elaiza-request'.

Send PROMPT and SYSTEM-PROMPT to BACKEND.
After a successful request ON-SUCCESS is called.
For resolving the streamed response ON-STREAMED-RESPONSE is used.
When the last chunk was sent ON-END is called.
"
  (let ((url-request-method "POST")
        request-buffer)
    (cl-multiple-value-bind (url url-request-extra-headers url-request-data)
        (elaiza-request--encode prompt system-prompt backend)
      (elaiza-debug 'request "Request to %s\n%S\n%s" url url-request-extra-headers url-request-data)
      (setq request-buffer (url-retrieve url on-success 'nil))
      (with-current-buffer request-buffer
        (add-hook 'after-change-functions
                  (elaiza-request--after-change-function on-streamed-response backend on-end)
                  nil
                  t))
      (when elaiza-buffer
        (with-current-buffer elaiza-buffer
          (setq-local elaiza-request--buffer request-buffer)))
        request-buffer)))

(defun elaiza-request (prompt system-prompt on-success on-streamed-response backend &optional elaiza-buffer on-end)
  "Send PROMPT and SYSTEM-PROMPT to BACKEND.

Run (async) `elaiza-request--after-change-function' before request.
For example, to check if the backend is online.
After a successful request ON-SUCCESS is called.
For resolving the streamed response ON-STREAMED-RESPONSE is used.
When the last chunk was sent ON-END is called."
  (let ((pre-request (elaiza-backend-pre-request-function backend))
        (callback (lambda ()
                    (elaiza-debug 'elaiza-request "Making request")
                    (funcall #'elaiza--request prompt system-prompt on-success on-streamed-response backend elaiza-buffer on-end))))
    (if pre-request
      (funcall pre-request backend callback)
      (funcall callback))))

(defun elaiza-request--after-change-function (on-streamed-response backend &optional on-end)
  "Function is intended to be used as `after-change-functions' hook.

ON-STREAMED-RESPONSE: Callback with the response string as argument.
ON-END: Callback when the response is complete.
Parsing is BACKEND specific."
  (lambda (beg end _)
    ;; (elaiza-debug 'request (buffer-substring-no-properties beg end))
    (elaiza-request--process-streamed-response beg end on-streamed-response backend on-end)))

(defun elaiza-request--process-streamed-response (beg end on-streamed-response backend &optional on-end)
  "Process streamed response from BEG to END via BACKEND.
Parsed response is passed to ON-STREAMED-RESPONSE callback.
ON-END: Callback when the response is complete.

Check response line-by-line for text delta.
BEG and END refer to `after-change-functions' hook's arguments."
  (goto-char beg)
  (while (< (point) end)
    (when-let ((message-delta (buffer-substring-no-properties (point) (progn (end-of-line) (point))))
               (result (elaiza-request--parse-streamed-response message-delta backend on-end)))
      ;; Mark LLM text for a continued conversation.
      (add-text-properties 0 (length result) '(role "assistant") result)
      (funcall on-streamed-response result))
    (forward-line 1)))

(cl-defgeneric elaiza-request--parse-streamed-response (message-delta backend &optional on-end)
  "Parse MESSAGE-DELTA of the request buffer from streamed response with BACKEND.
Call ON-END if last message.")

(defun elaiza-request-url-copy-file (url newname &optional ok-if-already-exists callback)
  "Async implementation of `url-copy-file': copy URL to NEWNAME.
Call CALLBACK if file NEWNAME already exists and should not be overwritten.
Overwrite if OK-IF-ALREADY-EXISTS.
Call CALLBACK after successful download."
  (if (and (file-exists-p newname)
           (not (or ok-if-already-exists
                    (yes-or-no-p (format "File %s already exists; copy to it anyway? "
                                         newname)) )))
      (when callback (funcall callback))
    (let* ((show-status url-show-status)
           (buffer (url-retrieve url
                                 (lambda (status)
                                   (setq url-show-status show-status)
                                   (if (plist-get status :error)
                                       (error "Failed to retrieve data: %S" (plist-get status :error))
                                     (let* ((buffer (current-buffer))
                                            (handle (with-current-buffer buffer
                                                      (mm-dissect-buffer t)))
                                            (mm-attachment-file-modes (default-file-modes)))
                                       (mm-save-part-to-file handle newname)
                                       (kill-buffer buffer)
                                       (mm-destroy-parts handle)
                                       (elaiza-debug 'elaiza-request-url-copy-file "Download completed.")
                                       (when callback (funcall callback)))))
                                 nil
                                 nil))
           ;; TODO We could make the watcher react only on `buffer'.
           (watcher (lambda (_symbol _newval _operation _where) (setq url-show-status t))))
      (with-current-buffer buffer
        (message "Downloading %s from %s..." newname url)
        ;; We want to see the progress when downloading a multi-GB model.
        ;; But `url-show-status' is set to nil for a new request
        ;; If, e.g., a CDN redirects our request we have to set it again.
        (add-variable-watcher 'url-http-after-change-function watcher))))
  nil)

(provide 'elaiza-request)
;;; elaiza-request.el ends here
