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
;; SPDX-License-Identifier: GPL-3.0-only
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  `ELAIZA' is a LLM agnostic assistant based on the pioneering ELIZA program used inside Emacs' `doctor'.
;;
;;; Code:
(require 'cl-lib)
(require 'elaiza-openai)
(require 'elaiza-claude)
(require 'elaiza-llamafile)
(require 'elaiza-ollama)
(require 'elaiza-backends)

;; Variables
(defgroup elaiza
  nil
  "Use an LLM assistant."
  :group 'external
  :prefix 'elaiza)
(defcustom elaiza-system-prompt
  "Your name is ELAIZA. ELAIZA is based on the early AI program ELIZA that is part of Emacs' doctor."
  "Default system prompt for ELAIZA. Prefix for other system prompts.

For a guide to system prompts see https://matt-rickard.com/a-list-of-leaked-system-prompts."
  :type 'string)

(setq elaiza-backends-integrations-alist 'nil)
(elaiza-backends--add-integration (make-elaiza-llamafile))
(elaiza-backends--add-integration (make-elaiza-gpt-4-turbo))
(elaiza-backends--add-integration (make-elaiza-gpt-4))
(elaiza-backends--add-integration (make-elaiza-gpt-3.5-turbo))
(elaiza-backends--add-integration (make-elaiza-ollama))
(elaiza-backends--add-integration (make-elaiza-claude-opus))
(elaiza-backends--add-integration (make-elaiza-claude-sonnet))
(elaiza-backends--add-integration (make-elaiza-claude-haiku))

(defcustom elaiza-available-backends elaiza-backends-integrations-alist
  "Available ELIZA backends.
See `elaiza-backends-integrations-alist' for a list of supported backends."
  :group 'elaiza
  :type '('string . 'elaiza-backend))



;; Functions

(defun elaiza-query-prompt ()
  "Query for PROMPT in the mini buffer."
  (interactive)
  (read-from-minibuffer "Prompt: "))

(defun elaiza-query-backend ()
  "Query for backend name in the mini buffer."
  (interactive)
  (completing-read "LLM: " elaiza-available-backends 'nil 't 'nil 'nil 'nil))

(provide 'elaiza)
;;; elaiza.el ends here

