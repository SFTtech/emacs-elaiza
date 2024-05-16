;;; elaiza.el --- A LLM agnostic assistant  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Alessandro Wollek
;;
;; Author: Alessandro Wollek <contact@wollek.ai>
;; Maintainer: Alessandro Wollek <contact@wollek.ai>
;; Created: April 05, 2024
;; Modified: May 12, 2024
;; Version: 0.0.2
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/SFTtech/emacs-elaiza
;; SPDX-License-Identifier: GPL-3.0-only
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  `ELAIZA' is a LLM agnostic assistant based on the pioneering ELIZA program used inside Emacs' `doctor'.
;;
;;; Code:

(defgroup elaiza nil
  "Use an LLM assistant."
  :group 'external)

(defcustom elaiza-system-prompt
  "Your name is ELAIZA. ELAIZA is based on the early AI program ELIZA that is part of Emacs' doctor."
  "Default system prompt for ELAIZA. Prefix for other system prompts.

For a guide to system prompts see https://matt-rickard.com/a-list-of-leaked-system-prompts."
  :group 'elaiza
  :type 'string)

(require 'cl-lib)
(require 'elaiza-openai)
(require 'elaiza-claude)
(require 'elaiza-llamafile)
(require 'elaiza-ollama)
(require 'elaiza-backends)

(defcustom elaiza-available-backends nil
  "Available ELAIZA backends.
See `elaiza-backends-integrations-alist' for a list of supported backends."
  :group 'elazia
  :type 'list)

(defcustom elaiza-default-model elaiza-llamafile-default-model
  "Default model."
  :group 'elaiza
  :type '(sexp :validate 'elaiza-backend-p))

;;;###autoload
(defun elaiza-change-default-model ()
  "Change the `elaiza-default-model'.
For a permanent change customize `elaiza-default-model'.

For example, for GPT-4 Turbo:
`(use-package elaiza
  :config (setq elaiza-default-model (make-elaiza-gpt-4-turbo))'"
  (interactive)
  (setq elaiza-default-model (elaiza-query-backend nil t)))

(defun elaiza-load-all-integrations ()
  "Load all elaiza integrations.

Ollama and llamafile need to be configured.
They are dependent on locally available models.

For example you can add an ollama integration (Llama3 70B) as follows:
`(elaiza-backends--add-integration (make-elaiza-ollama :name \"Llama3 70B\" :model \"llama3:70b\"))'.

Similarly for a Llamafile:
`(elaiza-backends--add-integration (make-elaiza-llamafile
 :name \"Llamafile: TinyLlama 1.1B\"
 :filename \"~/llamafiles/tinyllama-1.1B.llamafile\"
 :url \"https://huggingface.co/jartine/TinyLlama-1.1B-Chat-v1.0-GGUF/resolve/main/TinyLlama-1.1B-Chat-v1.0.F16.llamafile?download=true\")
)'"
  (elaiza-backends--add-integration elaiza-default-model)
  (elaiza-backends--add-integration (make-elaiza-gpt-4o))
  (elaiza-backends--add-integration (make-elaiza-gpt-4-turbo))
  (elaiza-backends--add-integration (make-elaiza-gpt-4))
  (elaiza-backends--add-integration (make-elaiza-gpt-3.5-turbo))
  (elaiza-backends--add-integration (make-elaiza-claude-opus))
  (elaiza-backends--add-integration (make-elaiza-claude-sonnet))
  (elaiza-backends--add-integration (make-elaiza-claude-haiku)))

(defun elaiza-add-available-backend (backend)
  "Add available BACKEND."
  (unless (assoc (elaiza-backend-name backend) elaiza-available-backends)
    (push (cons (elaiza-backend-name backend) backend)
          elaiza-available-backends)))

(defun elaiza-query-prompt ()
  "Query for PROMPT in the mini buffer."
  (interactive)
  (read-from-minibuffer "Prompt: "))

(defun elaiza-query-backend (&optional backend select _prefix)
  "Query for BACKEND when called with PREFIX `C-u'
or SELECT is non-nil.

If no backend was chosen use `elaiza-default-model'."
  (if (or current-prefix-arg select)
      (progn
        (unless elaiza-available-backends
          (elaiza-load-all-integrations)
          (setq elaiza-available-backends elaiza-backends-integrations-alist))
        (setq backend
              (cdr (assoc (completing-read
                           "LLM: "
                           elaiza-available-backends
                           nil t nil nil nil)
                          elaiza-available-backends))))
    (unless backend (setq backend elaiza-default-model)))
  backend)

(provide 'elaiza)
;;; elaiza.el ends here

