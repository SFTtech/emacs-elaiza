;;; elaiza.el --- A LLM agnostic assistant  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Alessandro Wollek
;;
;; Author: Alessandro Wollek <contact@wollek.ai>
;; Maintainer: Alessandro Wollek <contact@wollek.ai>
;; Created: April 05, 2024
;; Modified: April 26, 2024
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/SFTtech/emacs-elaiza
;; SPDX-License-Identifier: AGPL-3.0-only
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  `ELAIZA' is a LLM agnostic assistant based on the pioneering ELIZA program used inside Emacs' `doctor'.
;;
;;; Code:
(require 'cl-lib)
(require 'elaiza-request)
(require 'elaiza-claude)

;; Variables
(defgroup elaiza
  nil
  "Use an LLM assistant."
  :group 'external
  :prefix 'elaiza)

(defvar elaiza-debug 't "Log for debugging purposes to *elaiza log*.")

(cl-defstruct elaiza-backend
  "Struct for backend-specific dispatching."
  name)

(defvar elaiza-available-backends
  (mapcar (lambda (backend)
            (cons (format "%s" (elaiza-backend-name backend)) backend))
          (list (make-elaiza-claude)))
  "LLM backends that are available to ELAIZA.")

(defvar elaiza-system-prompt
  "Your name is ELAIZA based on the early AI program ELIZA that is now part of Emacs' doctor.
 Your answer should be formatted using Emacs org-mode syntax.
Always begin your response with '# ELAIZA:\n\n'.
Do not state this information."
  "For a guide to system prompts see https://matt-rickard.com/a-list-of-leaked-system-prompts.")


;; Functions
;;;###autoload
(defun elaiza (prompt backend-name)
  "Chat with ELAIZA.

Send PROMPT to LLM (BACKEND-NAME)."

  (interactive (list (read-from-minibuffer "Prompt: ")
                     (completing-read "LLM: " elaiza-available-backends 'nil 't 'nil 'nil 'nil)))
  (switch-to-buffer-other-window
   (get-buffer-create (generate-new-buffer-name
                       (concat "*elaiza: " (substring prompt 0 (min (length prompt) 20)) "*"))))
  (elaiza-mode)
  ;; Store the utilized LLM so we do not need to requery on `elaiza-continue-chat'.
  (setq-local elaiza--backend (cdr (assoc backend-name elaiza-available-backends)))
  (add-text-properties 0 (length prompt) '(role "user") prompt)
  (insert "#+TITLE: " prompt "\n\n")
  (elaiza--send (list `((:role . "user") (:content . prompt))) elaiza--backend))

(defun elaiza-continue-chat ()
  "Continue conversation inside *elaiza* buffer."
  (interactive)
  (if elaiza--backend
      (progn
        (insert "\n")
        (elaiza--send (elaiza--split-text-by-role) elaiza--backend))
    (message "Are you in an *elaiza* buffer?")))

(defun elaiza-kill-all-buffers ()
  "Kill all *elaiza* buffers."
  (interactive)
  (kill-matching-buffers "\*elaiza: "))

(defun elaiza--send (prompt backend)
  "Send PROMPT to BACKEND."
  (let ((elaiza-buffer (current-buffer))
        (start (point)))
    (elaiza-request prompt
                    elaiza-system-prompt
                    ;; on-success
                    (lambda (_)
                      (setq org-element-use-cache 't)
                      (when elaiza-debug
                        (let ((response (buffer-substring-no-properties (point-min) (point-max))))
                          (with-current-buffer (get-buffer-create "*elaiza-log*")
                            (goto-char (point-max))
                            (insert "Response length %d" (length response)
                                    "\n\n"
                                    response)))))
                    ;; on-streamed-response
                    (lambda (response-delta)
                      (with-current-buffer elaiza-buffer
                        (when response-delta
                          (save-excursion
                            (goto-char start)
                            (insert response-delta)
                            (setq start (point))))))
                    backend)
    ;; Partial insertions cause org-element parsing errors.
    (setq org-element-use-cache 'nil)))

(defun elaiza--mark-user-input (beg end length-before)
  "Add user role text property to text between BEG and END.

Used as part of `after-change-functions' hook. LENGTH-BEFORE not used."
  (add-text-properties beg end '(role "user")))

(defun elaiza--split-text-by-role ()
  "Split the current buffer's text by the `role' text property until `point'."
  (save-excursion
    (let ((result '())
          (start (point-min))
          (end (point))
          role)
      (goto-char (point-min))
      (while (and (text-property-search-forward 'role )
                  (< start end))
        (setq role (get-text-property start 'role))
        (push (list (cons :role role)
                    (cons :content (buffer-substring-no-properties
                                    start (min (point) end)))) result)
        (setq start (point)))
      (nreverse result))))

(define-derived-mode elaiza-mode org-mode "ELAIZA"
  "Major mode for interacting with an LLM via ELAIZA."
  :interactive 'nil
  (turn-on-auto-fill)
  (add-hook 'after-change-functions #'elaiza--mark-user-input))

(provide 'elaiza)
;;; elaiza.el ends here
