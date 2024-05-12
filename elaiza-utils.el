;;; elaiza-utils.el --- Common utilities for ELAIZA -*- lexical-binding: t; -*-
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
;;  Common utilities for ELAIZA
;;
;;; Code:

(defcustom elaiza-debug nil
  "Log for debugging purposes to *elaiza log*.

For debugging network connections set variable `url-debug' to non-nil."
  :group 'elaiza
  :type 'boolean)

(defun elaiza-debug (tag &rest args)
  "Append TAG -> ARGS to *elaiza-log* if `elaiza-debug' is non-nil."
  (when elaiza-debug
      (with-current-buffer (get-buffer-create "*elaiza-log*")
      (save-excursion
	(goto-char (point-max))
	(insert (symbol-name tag) " -> " (apply #'format args) "\n")))))
(provide 'elaiza-utils)
;;; elaiza-utils.el ends here
