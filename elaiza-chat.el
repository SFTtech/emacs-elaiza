;;; elaiza-chat.el --- Chatting with ELAIZA -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Alessandro Wollek
;;
;; Author: Alessandro Wollek <a@wollek.dev>
; Copyright (C) 2024 Alessandro Wollek
;;
;; Author: Alessandro Wollek <contact@wollek.ai>
;; Homepage: https://github.com/SFTtech/emacs-elaiza
;; SPDX-License-Identifier: GPL-3.0-only
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Chatting with ELAIZA
;;
;;; Code:
(require 'org)
(require 'elaiza)

(defvar-keymap elaiza-mode-map
  :parent org-mode-map
  "C-c RET" #'elaiza-chat-continue
  "C-c <RET>" #'elaiza-chat-continue)

(defcustom elaiza-chat-system-prompt
  (concat elaiza-system-prompt "Your INITIAL response is the beginning of the org document and starts with #+TITLE:.
Start with the appropriate title first.
Follow-up responses should start with #+ELAIZA:
IMPORTANT: You will reply only in the Emacs ORG-MODE format.
DO NOT USE MARKDOWN.")
  "For a guide to system prompts see https://matt-rickard.com/a-list-of-leaked-system-prompts."
  :group 'elaiza
  :type 'string)

;;;###autoload
(defun elaiza-chat (&optional prompt backend system-prompt buffer-name discard-prompt)
  "Chat with ELAIZA.

Send PROMPT to llm with a custom SYSTEM-PROMPT.
Select LLM when prefixed with `C-u'.
Save prompt as property unles DISCARD-PROMPT is non-nil.
Show chat in BUFFER-NAME."
  (interactive "sPrompt: \nP")
  (unless buffer-name (setq buffer-name (generate-new-buffer-name
                       (concat "*elaiza: " (substring prompt 0 (min (length prompt) 20)) "*"))))
  (switch-to-buffer-other-window (get-buffer-create buffer-name))
  (elaiza-mode)
  (unless system-prompt (setq system-prompt (default-value 'elaiza-chat-system-prompt)))
  ;; Store the utilized LLM so we do not need to requery on `elaiza-continue-chat'.
  (setq-local elaiza--backend (elaiza-query-backend backend))
  (setq-local elaiza-system-prompt system-prompt)
  (add-text-properties 0 (length prompt) '(role "user") prompt)
  (unless discard-prompt
    (insert ":PROPERTIES:
:PROMPT: " prompt
"\n:END:\n"))
  ;; Partial insertions cause org-element parsing errors.
  (when (boundp 'org-element-use-cache)
    (setq-local org-element-use-cache 'nil))
  (elaiza-debug 'elaiza-chat "%s -> %s" prompt (elaiza-backend-name elaiza--backend))
  (elaiza-chat--send (list `((role . "user") (content . ,prompt))) system-prompt elaiza--backend))


(defun elaiza-chat-continue ()
  "Continue conversation inside *elaiza* buffer."
  (interactive)
  (if (boundp 'elaiza--backend)
      (progn
        (insert "\n")
        (elaiza-chat--send (elaiza-chat--split-text-by-role) elaiza-system-prompt elaiza--backend))
    (message "Are you in an *elaiza* buffer?")))

(defun elaiza-chat--insert-response (response buffer point)
  "Insert RESPONSE into BUFFER at POINT.
Return POINT after insertion"
  (elaiza-debug 'elaiza-chat--insert-response response)
  (with-current-buffer
      buffer
    (when response
      (save-excursion
        (goto-char point)
        (insert response)
        (point)))))

(defun elaiza-chat--send (prompt system-prompt backend)
  "Send PROMPT and SYSTEM-PROMPT to BACKEND."
  (elaiza-debug 'elaiza--send "%S" prompt)
  (let ((elaiza-buffer (current-buffer))
        (start (point)))
    (elaiza-request
     prompt
     system-prompt
     ;; on-success
     (lambda (status)
       (elaiza-debug 'elaiza-chat--send "Status %S" status)
       (with-current-buffer elaiza-buffer
         (when (boundp 'elaiza--request-buffer)
           (setq elaiza--request-buffer nil)))
       (when (plist-get status :error)
         (message (buffer-substring-no-properties (point-min) (point-max)))
         (error "Elaiza request error")))
     ;; on-streamed-response
     (lambda (response-delta)
       (let ((request-buffer (current-buffer)))
         (with-current-buffer elaiza-buffer
           (setq-local elaiza--request-buffer request-buffer)))
       (setq start (elaiza-chat--insert-response response-delta elaiza-buffer start)))
     backend)))

(defun elaiza-chat-kill-all-buffers (&optional no-ask)
  "Kill all *elaiza* chat buffers.
Asks before killing each buffer, unless NO-ASK is non-nil."
  (interactive "P")
  (kill-matching-buffers "\*elaiza: " 'nil no-ask))

(defun elaiza-chat--split-text-by-role ()
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

(defun elaiza-chat--mark-user-input (beg end _)
  "Add user role text property to text between BEG and END.

Used as part of `after-change-functions' hook. LENGTH-BEFORE not used."
  (add-text-properties beg end '(role "user")))

(define-derived-mode elaiza-mode org-mode "ELAIZA"
  "Major mode for interacting with an LLM via ELAIZA."
  :interactive 'nil
  (turn-on-auto-fill)
  ;; Keep track of assistant and user text.
  (add-hook 'after-change-functions #'elaiza-chat--mark-user-input nil t))

(provide 'elaiza-chat)
;;; elaiza-chat.el ends here
