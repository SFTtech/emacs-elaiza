;;; elaiza-editor.el --- Elaiza Spell Checker and Editor -*- lexical-binding: t; -*-
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
;;  Elaiza Spell Checker and Editor.
;;  Similar to OpenAI's GPTs this uses Elaiza as an editor.
;;
;;; Code:
(require 'elaiza)

(defcustom elaiza-editor-system-prompt "
You are a multilingual expert editor.
Below you can find a draft.
Do not repeat the drafted text, provide only suggestions.

Proof-read the text giving detailed suggestions regarding spelling, grammar,
 proper sentence structure and punctuation.
Make sure your suggestions in the same language as the provided draft.
Pay attention that your suggestions fit the context of the given draft.
In case it is source code or uses a special syntax, edit only the prose text.
If the draft has no errors state just that.

The draft you have to edit is:\n"
  "System prompt for the ELAIZA editor."
  :group 'elaiza
  :type 'string)

;;;###autoload
(defun elaiza-editor (backend-name)
  "Send current buffer to BACKEND-NAME and provide editing suggestions."
  (interactive (list (completing-read "LLM: " elaiza-available-backends 'nil 't 'nil 'nil 'nil)))
  (let ((current-buffer-name (buffer-name))
        (current-buffer-content (buffer-substring-no-properties (point-min) (point-max))))
  (switch-to-buffer-other-window (get-buffer-create (generate-new-buffer-name (concat
                                                                  "*elaiza editor* "
                                                                  current-buffer-name))))
  (elaiza-mode)
  (setq-local elaiza-system-prompt elaiza-editor-system-prompt)
  (setq-local elaiza--backend (cdr (assoc backend-name elaiza-available-backends)))
  (elaiza--send (list `((role . "user") (content . ,current-buffer-content))) elaiza--backend)))

(provide 'elaiza-editor)
;;; elaiza-editor.el ends here
