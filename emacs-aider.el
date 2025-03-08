;;; -*- lexical-binding: t -*-

;; Author:  xplutoyz
;; Created: 2025-03-08 13:10:34

;;; Commentary:
;; Emacs integration for aider.chat - an AI pair programming assistant

;;; Code:

(require 'comint)
(require 'json)

(defgroup emacs-aider nil
  "Emacs integration for aider.chat"
  :group 'tools)

(defcustom emacs-aider-command "aider"
  "The command to run the aider chat interface."
  :type 'string
  :group 'emacs-aider)

(defcustom emacs-aider-buffer-name "*aider*"
  "Name of the buffer for the aider chat interface."
  :type 'string
  :group 'emacs-aider)

(defun emacs-aider--start-process ()
  "Start the aider process."
  (let ((buffer (get-buffer-create emacs-aider-buffer-name)))
    (unless (comint-check-proc buffer)
      (apply 'make-comint-in-buffer "aider" buffer emacs-aider-command nil))
    buffer))

;;;###autoload
(defun emacs-aider ()
  "Start or switch to the aider chat interface."
  (interactive)
  (let ((buffer (emacs-aider--start-process)))
    (pop-to-buffer buffer)
    (unless (eq major-mode 'comint-mode)
      (comint-mode))))



(provide 'emacs-aider)
;;; emacs-aider.el ends here
