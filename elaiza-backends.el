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
  name)

(defvar elaiza-backends-integrations-alist 'nil
  "LLM backends that are incorporated into ELAIZA.")

(defun elaiza-backends--add-integration (backend)
  (setq elaiza-backends-integrations-alist
        (cons (cons (format "%s" (elaiza-backend-name backend))
                    backend)
              elaiza-backends-integrations-alist)))

(provide 'elaiza-backends)
;;; elaiza-backends.el ends here
