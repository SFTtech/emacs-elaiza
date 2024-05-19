;;; elaiza-llamafile.el --- Implementation for interacting with a llamafile -*- lexical-binding: t; -*-
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

(require 'async)
(require 'cl-lib)
(require 'executable)
(require 'json)
(require 'url-http)
(require 'elaiza-backends)
(require 'elaiza-request)

(defcustom elaiza-llamafile-max-attempts 5
  "Number of attempts to start the Llamafile server."
  :group 'elaiza
  :type 'number)

;;;###autoload
(defun elaiza-llamafile-start (&optional prefix llamafile-backend download callback n)
  "Start Llamafile server.

DOWNLOAD LLAMAFILE-BACKEND if it non-nil and does not exist, call CALLBACK with filename.

See https://github.com/Mozilla-Ocho/llamafile/blob/790029485fdd3a81284efdcae1c0483a4d39a6a6/llama.cpp/server/README.md#api-endpoint
for a list of options.

To start a LLAMAFILE-BACKEND with minimal user interaction:
- call CALLBACK if LLAMAFILE-BACKEND is already running
otherwise
- check if LLAMAFILE-BACKEND exists
- download LLAMAFILE-BACKEND if not and DOWNLOAD is non-nil
- start LLAMAFILE-BACKEND server and call CALLBACK

When PREFIX is non-nil prompt the user to select a Llamafile.
Callbacks are necessary as all url-requests are async.
Keep track of the Nth startup attempt.

When calling `elaiza-chat' from a clean install, without any configuration,
`elaiza-llamafile-start' is called as `elaiza-backend-pre-request-function'.
This ensures that the default llamafile is downloaded and the server started.
The async nature makes this process convoluted:
1. The CALLBACK of `elaiza-llamafile-start' is an `elaiza--request'.
This was envoced by calling `elaiza-chat', calling `elaiza-request'.
2. Ping the llamafile server. If it is reachable call CALLBACK.
3. If not check if the llamafile exists (`elaiza-llamafile--start').
4. (If not download the llamafile first (`elaiza-llamafile-download')
    and call `elaiza-llamafile--start' again.)
5. Start the llamafile server
   and ping it N times with a delay of `elaiza-llamafile-startup' seconds.
   GOTO 2."
  (interactive "P")
  (setq llamafile-backend
        (if prefix
            (elaiza-llamafile-select-model)
          (elaiza-llamafile--backend-or-default llamafile-backend)))
  (elaiza-debug 'llamafile-start "Using backend %s (%s)" (elaiza-llamafile-name llamafile-backend)
                (elaiza-llamafile-filename llamafile-backend))
  (unless n
    (setq n 0))
  (unless (< n elaiza-llamafile-max-attempts)
    (error "Could not start %s (%s)" (elaiza-llamafile-name llamafile-backend)
           (elaiza-llamafile-filename llamafile-backend)))
  (elaiza-llamafile-ping
   llamafile-backend
   (lambda (running)
     (elaiza-debug 'llamafile-start "Llamafile server running: %S" running)
     (if (not running)
         (elaiza-llamafile--start
          llamafile-backend
          download
          callback
          n)
       (when callback
         (elaiza-debug 'llamafile-start "Llamafile already running. Calling callback.")
         (funcall callback))))))

(cl-defstruct (elaiza-llamafile
               (:include elaiza-backend
                         (pre-request-function
                          (lambda (backend callback)
                            (elaiza-debug 'pre-request-function "calling callback.")
                            (elaiza-llamafile-start nil backend t callback 0)))))
  "Generic Llamafile."
  (filename nil :type string)
  (url nil :type string)
  (host "127.0.0.1" :type string)
  (port "8080" :type string))

(cl-defstruct
    (elaiza-llamafile-tinyllama-1.1B
     (:include elaiza-llamafile
               (name "Llamafile: TinyLlama 1.1B")
               (url "https://huggingface.co/jartine/TinyLlama-1.1B-Chat-v1.0-GGUF/resolve/main/TinyLlama-1.1B-Chat-v1.0.F16.llamafile?download=true")))
  "TinyLlama-1.1B. Requires 2.05GB.
See https://huggingface.co/jartine/TinyLlama-1.1B-Chat-v1.0-GGUF.")

(cl-defstruct
    (elaiza-llamafile-llava_1.5
     (:include elaiza-llamafile
               (name "Llamafile: LLaVa 1.5")
               (url "https://huggingface.co/Mozilla/llava-v1.5-7b-llamafile/resolve/main/llava-v1.5-7b-q4.llamafile?download=true")))
  "LLava 1.5. Requires 3.97 GB.
See https://huggingface.co/Mozilla/llava-v1.5-7b-llamafile.")

(cl-defstruct
    (elaiza-llamafile-rocket-3B
     (:include elaiza-llamafile
               (name "Llamafile: Rocket-3B")
               (url "https://huggingface.co/Mozilla/rocket-3B-llamafile/resolve/main/rocket-3b.Q5_K_M.llamafile?download=true")))
  "Rocket-3B. Requires 1.89 GB.
See https://huggingface.co/Mozilla/rocket-3B-llamafile.")

(defcustom elaiza-llamafile-directory "~/llamafiles/"
  "Default directory to download Llamafiles."
  :group 'elaiza
  :type 'directory)

(defcustom elaiza-llamafile-startup "10"
  "Waiting time until Llamafile server (hopefully) responsive."
  :group 'elaiza
  :type 'string)

(defcustom elaiza-llamafile-default-model
  (progn
    (declare-function make-elaiza-llamafile-rocket-3B "elaiza-llamafile")
    (make-elaiza-llamafile-rocket-3B
     :filename (concat elaiza-llamafile-directory "rocket-3B.llamafile")))
  "Default Llamafile model.
Requires a URL, a FILENAME and a NAME."
  :group 'elaiza
  :type '(sexp :validate 'elaiza-llamafile-p))

(cl-defmethod elaiza-request--encode (messages system-prompt (backend elaiza-llamafile))
  "Send MESSAGES to Llamafile BACKEND and pass response to CALLBACK.

See https://platform.openai.com/docs/api-reference/chat
Add SYSTEM-PROMPT if non-nil.
See https://github.com/ggerganov/llama.cpp/blob/master/examples/server/README.md#change-system-prompt-on-runtime"
  (elaiza-debug 'elaiza-request--encode "%S" messages)
  (let ((headers '(("content-type" . "application/json")
                   ("accept-charset" . "utf-8")))
        (body (list (cons 'messages (if system-prompt
                                        (append
                                         (list (list (cons 'role "system")
                                                     (cons 'content system-prompt)))
                                         messages)
                                      messages))
                    (cons 'stream  't)))
        (url (concat "http://"
                     (elaiza-llamafile-host backend)
                     ":"
                     (elaiza-llamafile-port backend)
                     "/v1/chat/completions")))
    (list url headers (encode-coding-string (json-encode body) 'utf-8))))

(cl-defmethod elaiza-request--parse-streamed-response (message-delta (_ elaiza-llamafile))
  "Parse a partial stream response (MESSAGE-DELTA) from ELAIZA-BACKEND Llamafile."
  (when (and message-delta
             (string-match "\"delta\":{\"content\":\\(.*?\\)}" message-delta))
    (decode-coding-string (json-read-from-string (match-string 1 message-delta)) 'utf-8)))

(defun elaiza-llamafile-download (&optional url filename callback)
  "Download llamafile (FILENAME) from URL.
Call CALLBACK on success."
  (interactive)
  (elaiza-debug 'llamafile-download url)
  (make-directory elaiza-llamafile-directory t)
  (elaiza-request-url-copy-file url filename nil callback))

(defun elaiza-llamafile-select-model ()
  "Read Llamafile and return backend.
Adds backend to `elaiza-available-backends'."
  (interactive)
  (let* ((filename (read-file-name "Llamafile: "))
         (llamafile (make-elaiza-llamafile
         :name (file-name-base filename)
         :filename filename
         :url 'nil)))
    (elaiza-debug 'llamafile-select-model "Selected %S" llamafile)
    (elaiza-add-available-backend llamafile)
    llamafile))

(defun elaiza-llamafile--backend-or-default (&optional llamafile-backend)
  "Use default LLAMAFILE-BACKEND or query if nil."
    (if llamafile-backend llamafile-backend elaiza-llamafile-default-model))

(defun elaiza-llamafile-running-p ()
  "Return non-nil if the Llamafile server is running.
Assumes it was started using `elaiza-llamafile-start'.
Otherwise use `elaiza-llamafile-ping'"
  (not (null (process-status "elaiza-llamafile"))))

(defun elaiza-llamafile--health (llamafile-backend callback)
  "Check health endoint of LLAMAFILE-BACKEND.
Call CALLBACK with response."
  (let ((url (format "http://%s:%s/health"
                     (elaiza-llamafile-host llamafile-backend)
                     (elaiza-llamafile-port llamafile-backend))))
  (elaiza-debug 'llamafile-health "Pinging %s" url)
  (url-retrieve url
   (lambda (status)
     (elaiza-debug 'llamafile-health "Result %S" status)
     (if (plist-get status :error)
         (funcall callback nil)
       (funcall callback t)))
   nil t)
  nil))

(defun elaiza-llamafile-ping (llamafile-backend callback )
  "Check if LLAMAFILE-BACKEND is online.
Use `elaiza-llamafile-default-model' if LLAMAFILE-BACKEND is nil.
Call CALLBACK if LLAMAFILE-BACKEND status as argument."
  (elaiza-debug 'elaiza-llamafile-ping "Llamafile server status %S" (elaiza-llamafile-running-p))
  (if (not (elaiza-llamafile-running-p))
      (funcall callback nil)
    (elaiza-llamafile--health  llamafile-backend callback)))

(defun elaiza-llamafile--start (llamafile-backend download callback n)
  "Start the LLAMAFILE-BACKEND server without pinging.
DOWNLOAD Llamafile if not exist and non-nil.
After starting, call CALLBACK via `elaiza-llamafile-start'
and increase startup counter N.
CALLBACK is an `elaiza--request'.

If you want to start the server interactively see `elaiza-llamafile-start'."
  (let ((file (elaiza-llamafile-filename llamafile-backend)))
    ;; Check if Llamafile exists.
    (if (not (file-exists-p file))
        (if (and download
                 (yes-or-no-p (format "Llamafile %s does not exist; download? "
                                      file)))
            ;; Download Llamafile then start.
            (elaiza-llamafile-download
             (elaiza-llamafile-url llamafile-backend)
             file
             (lambda ()
               (elaiza-add-available-backend llamafile-backend)
               (elaiza-llamafile--start llamafile-backend nil callback 0)))
          (signal 'file-error (format "%s does not exist" file)))
      ;; We have a Llamafile. Let's start the server.
      (progn
        (unless (file-executable-p file)
          (set-file-modes file (logior executable-chmod (file-modes file))))
        (unless (elaiza-llamafile-running-p)
          (elaiza-debug 'llamafile--start "Starting %s..." file)
          (message "Starting %s..." file)
          (async-start-process "elaiza-llamafile"
                               "sh"
                               (lambda (_) (elaiza-debug 'llamafile "process ended."))
                               (expand-file-name file)
                               "-ngl"
                               "--unsecure"
                               "--nobrowser"))
        (async-start-process
         "elaiza-wait-for-startup"
         "sleep"
         ;; Starting the server takes some time.
         ;; Avoid a block call and wait `elaiza-llamafile-startup' seconds.
         (lambda (_)
           (elaiza-debug
            'llamafile--start
            "Slept %s seconds; checking if Llamafile server is now online."
            elaiza-llamafile-startup)
           (elaiza-llamafile-start nil llamafile-backend nil callback (1+ n)))
         elaiza-llamafile-startup)))))

(defun elaiza-llamafile-stop ()
  "Kill Llamafile process."
  (interactive)
  (kill-process "elaiza-llamafile")
  (elaiza-debug 'elaiza-llamafile "server stopped."))

(provide 'elaiza-llamafile)
;;; elaiza-llamafile.el ends here
