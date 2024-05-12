;;; elaiza-jupyter.el --- Elaiza Jupyter assistant  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Alessandro Wollek
;;
;; Author: Alessandro Wollek <contact@wollek.ai>
;; Homepage: https://github.com/SFTtech/emacs-elaiza
;; SPDX-License-Identifier: GPL-3.0-only
;;
;;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Elaiza Jupyter assistant.
;;  Uses a Python session throughout the conversation and stores Matplotlib plots to file.
;;
;;; Code:
(require 'elaiza-chat)

(defcustom elaiza-jupyter-system-prompt
  (concat elaiza-chat-system-prompt "\nNOTE:
- The python src blocks should use the session `:session s'
- Src blocks that should print something should have the results option
  `output', e.g. ~:results output~
- If you create a plot store it as a file. Below the source block display it
  using the org syntax for file links. As we are using org-mode you cannot use
  ~plt.show()~ clear the memory using the other methods available.")
  "Extended `org-mode' system prompt for ELAIZA."
  :group 'elaiza
  :type 'string)

;;;###autoload
(defun elaiza-jupyter ()
  "Chat with ELAIZA with special org-mode Python instructions."
  (interactive)
  (elaiza-chat 'nil 'nil elaiza-jupyter-system-prompt 'nil))

(provide 'elaiza-jupyter)
;;; elaiza-jupyter.el ends here
