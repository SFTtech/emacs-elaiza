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
;;  An updated Emacs Doctor
;;
;;; Code:
(require 'elaiza)

;;;###autoload
(defun elaiza-doctor (backend-name)
  "Switch to *doctor* buffer and start giving *enhanced* psychotherapy.

Uses BACKEND-NAME as LLM."
  (interactive (list (completing-read "LLM: " elaiza-available-backends 'nil 't 'nil 'nil 'nil)))
  (switch-to-buffer "*doctor*")
  (elaiza-doctor-mode)
  (setq-local elaiza--backend (cdr (assoc backend-name elaiza-available-backends))))

(defun elaiza-doctor-ret-or-read (arg)
  "Insert a newline if preceding character is not a newline.
Otherwise call the Doctor to parse preceding sentence."
  (interactive "*p" elaiza-doctor-mode)
  (if (= (preceding-char) ?\n)
      (progn (elaiza-continue-chat)
             (goto-char (point-max))
             (insert "\n\n"))
    (newline arg)))

(define-derived-mode elaiza-doctor-mode text-mode "ELAIZA Doctor"
  "Major mode for running the updated Doctor (ELAIZA) program.
Like Doctor mode but with AI."
  :interactive nil
  (setq elaiza-doctor-mode-map (make-sparse-keymap))
  (define-key elaiza-doctor-mode-map (kbd "RET") #'elaiza-doctor-ret-or-read)
  (setq-local elaiza-system-prompt "I am the psychotherapist.")
  (let ((prompt "I am the psychotherapist.  Please, describe your problems.  Each time you are
finished talking, type RET twice.\n\n"))
    (add-text-properties 0 (length prompt) '(role "assistant") prompt)
    (insert prompt))
  (add-hook 'after-change-functions #'elaiza--mark-user-input))

(provide 'elaiza-doctor)
;;; elaiza-doctor.el ends here