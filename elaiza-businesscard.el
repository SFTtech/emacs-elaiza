;;; elaiza-businesscard.el --- ELAIZA Businesscard Reader -*- lexical-binding: t; -*-
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
;;  Parsing a businesscard with ELAIZA and  GPT-4o mini.
;;
;;; Code:
(require 'elaiza-chat)

(defcustom elaiza-businesscard-default-dir
  nil
  "Default directory.")

;;;###autoload
(defun elaiza-businesscard (&optional file-path)
  "Parse the content of a businesscard.
FILE-PATH to the businesscard (pdf or jpg)."
  (interactive)
  (unless file-path
    (setq file-path (read-file-name "Path: " elaiza-businesscard-default-dir)))
  (unless (file-regular-p file-path)
    (error "The path '%s' is not a file" file-path))

  (let* ((file-path (expand-file-name file-path))
         (file-suffix (downcase (file-name-extension file-path)))
         (jpg-path (concat (file-name-sans-extension file-path) ".jpg"))
         (resized-path (concat (file-name-sans-extension file-path) "-resized.jpg")))
    ;; Convert PDF to JPG if necessary
    (when (string-equal file-suffix "pdf")
      (unless (file-exists-p jpg-path)
        (message "Converting '%s' to '%s'" file-path jpg-path)
        (let ((result (shell-command-to-string (format "magick %s %s"
                                                       (shell-quote-argument file-path)
                                                       (shell-quote-argument jpg-path)))))
          (unless (equal result "")
            (error "Error during conversion: %s" result)))))

    ;; Resize the image
    (unless (file-exists-p resized-path)
      (message "Resizing '%s' to 512x512" file-path)
      (let ((result (shell-command-to-string (format "magick %s -resize 512x512 %s"
                                                     (shell-quote-argument file-path)
                                                     (shell-quote-argument resized-path)))))
        (unless (equal result "")
          (error "Error during resizing: %s" result))))

    (setq file-path resized-path)

    ;; Encode image in base64
    (let* ((buffer-encoded (base64-encode-string
                            (with-temp-buffer
                              (insert-file-contents-literally file-path)
                              (buffer-string))))
           (start (point-max))
           (elaiza-buffer (get-buffer-create "*elaiza-businesscard*"))
           (elaiza--backend (make-elaiza-gpt-4o-mini :max_tokens 1000))
           (prompt `(((role . "user")
                      (content . ["This is a business card."
                                  "Extract all entities and label them. For example, first name, last name, company."
                                  "Return ONLY the result as a key-value list with the syntax:"
                                  "`- key :: value  `"
                                  "Example: "
                                  "`- First Name :: John`"
                                  "`- Last Name :: Doe`"
                                  (("image" . ,buffer-encoded))])))))

      (elaiza-request prompt
                      nil
                      ;; ON-SUCCESS
                      (lambda (_) (message "Parsing businesscard.."))
                      ;; ON-STREAMED-RESPONSE
                      (lambda (response-delta)
                        (setq start (elaiza-chat--insert-response response-delta elaiza-buffer start)))
                      elaiza--backend
                      elaiza-buffer
                      ;; ON-END
                      (lambda ()
                        (with-current-buffer elaiza-buffer
                          (let* ((name (elaiza-businesscard--extract-name-from-org-list))
                                 (buffer-name (concat "*businesscard-" (string-replace " " "-" name) "*")))
                            (switch-to-buffer (rename-buffer (generate-new-buffer-name buffer-name)))
                            (org-mode)
                            (goto-char (point-max))
                            (insert (format "\n#+attr_org: :width 300\n [[file:%s]]" jpg-path))
                            (insert (format "\n\n LinkedIn: [[https://linkedin.com/search/results/people/?keywords=%S][Search]]" name)))))))))

(defun elaiza-businesscard--extract-name-from-org-list ()
  "Extract names from an org list in the current buffer."
  (interactive)
  (let (first-name last-name)
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward "- First Name :: \\([[:alnum:]]+\\)" nil t)
        (setq first-name (match-string-no-properties 1)))
      (goto-char (point-min))
      (when (re-search-forward "- Last Name :: \\([[:alnum:]]+\\)" nil t)
        (setq last-name (match-string-no-properties 1))))
    (concat first-name " " last-name)))

(provide 'elaiza-businesscard)
;;; elaiza-businesscard.el ends here
