;;; elaiza-backends.el --- Available LLM backend implementations -*- lexical-binding: t; -*-
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
;;  All available LLM backend implementations.
;;
;;; Code:
(require 'cl-lib)

(cl-defstruct elaiza-backend
  "Struct for backend-specific dispatching."
  name
  (pre-request-function nil :type 'function))

(defvar elaiza-backends-integrations-alist nil
  "LLM backends that are incorporated into ELAIZA.")

(defvar elaiza--backend nil
  "Default elaiza backend for current buffer.")

(defun elaiza-backends--add-integration (backend)
  "Add an LLM BACKEND integration to ELAIZA."
  (unless (assoc (elaiza-backend-name backend) elaiza-backends-integrations-alist)
    (push (cons (elaiza-backend-name backend) backend)
          elaiza-backends-integrations-alist)))

(defcustom elaiza-available-backends nil
  "Available ELAIZA backends.
See `elaiza-backends-integrations-alist' for a list of supported backends."
  :group 'elazia
  :type 'list)

(defun elaiza-add-available-backend (backend)
  "Add BACKEND to available `elaiza-available-backends'."
  (unless (assoc (elaiza-backend-name backend) elaiza-available-backends)
    (push (cons (elaiza-backend-name backend) backend)
          elaiza-available-backends)))

(provide 'elaiza-backends)
;;; elaiza-backends.el ends here
