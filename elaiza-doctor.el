;;; elaiza-doctor.el --- An updated Emacs Doctor -*- lexical-binding: t; -*-
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
;;  An updated Emacs Doctor.
;;  Talk to an LLM instead of the original ELIZA doctor.
;;
;;; Code:
(require 'elaiza-chat)

(defvar elaiza-doctor-buffer "*doctor*"
  "Name of the Elaiza doctor buffer.")
(defvar elaiza-doctor-system-prompt "You are the Emacs Psychotherapist ELAIZA."
  "System prompt for the Emacs Psychotherapist.")

;;;###autoload
(defun elaiza-doctor (&optional _prefix)
  "Switch to *doctor* buffer and start giving *enhanced* psychotherapy.
Uses `elaiza-default-model' backend.
PREFIX function to choose an alternative backend for the session."
  (interactive "P")
  (switch-to-buffer elaiza-doctor-buffer)
  (elaiza-doctor-mode))

(defun elaiza-doctor-ret-or-read (&optional prefix)
  "Insert a newline if preceding character is not a newline.
Otherwise call the Doctor to parse preceding sentence.
Select backend when PREFIX is non-nil."
  (interactive "P")
  (if (= (preceding-char) ?\n)
      (progn (elaiza-chat-continue prefix)
             (goto-char (point-max))
             (insert "\n\n"))
    (newline)))

(define-derived-mode elaiza-doctor-mode text-mode "ELAIZA Doctor"
  "Major mode for running the updated Doctor (ELAIZA) program.
Like Doctor mode but with AI."
  :interactive "P"
  (setq elaiza-doctor-mode-map (make-sparse-keymap))
  (define-key elaiza-doctor-mode-map (kbd "<RET>") 'elaiza-doctor-ret-or-read)
  (setq-local elaiza-system-prompt elaiza-doctor-system-prompt)
  (setq-local elaiza--backend (elaiza-query-backend current-prefix-arg))
  (when (= (buffer-size (current-buffer)) 0)
    (let ((prompt "I am the psychotherapist. Please, describe your problems. Each time you are
finished talking, type RET twice.\n\n"))
      (insert prompt)
      (add-text-properties (point-min) (point-max) '(role "assistant"))))
  (add-hook 'after-change-functions #'elaiza-chat--mark-user-input nil t))

(provide 'elaiza-doctor)
;;; elaiza-doctor.el ends here
