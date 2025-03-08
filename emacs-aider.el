;;; emacs-aider.el --- Emacs integration for aider.chat -*- lexical-binding: t -*-

;; Copyright (C) 2025 xplutoyz

;; Author: xplutoyz
;; Created: 2025-03-08 13:10:34
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; Emacs integration for aider.chat - an AI pair programming assistant

;;; Code:

(require 'comint)
(require 'json)
(require 'project)

(defgroup emacs-aider nil
  "Emacs integration for aider.chat"
  :group 'tools)

(defcustom emacs-aider-command "aider"
  "The command to run the aider chat interface."
  :type 'string
  :group 'emacs-aider)

(defcustom emacs-aider-command-default-args "--model deepseek"
  "The command args for `emacs-aider-command'."
  :type 'string
  :group 'emacs-aider)

(defcustom emacs-aider-display-buffer-action nil
  "The default display action of `emacs-aider' buffer."
  :type 'list
  :group 'emacs-aider)

(defcustom emacs-aider-chat-window-selected-p t
  "whthere selected the chat window when start a new aider session."
  :type 'boolean
  :group 'emacs-aider)

(defun emacs-aider--run (buffer command cmd-args)
  "Start a new aider session."
  (apply #'make-comint-in-buffer "emacs-aider" buffer command nil cmd-args))

(defun emacs-aider--seed ()
  "Send command or contents to aider chat buffer."
  ;; TODO

  )

(defun emacs-aider--get-final-args ()
  "Build the final args of `emacs-aider-command'."
  ;; TODO
  ;; (format "%s" emacs-aider-command-default-args))
  (string-split emacs-aider-command-default-args nil))

(defun emacs-aider--get-buffer-name ()
  "Build `emacs-aider' buffer name according to `project-root' or `default-directory'."
  (format "*emacs-aider:%s*" (if-let* ((project (project-current)))
				  (project-root project)
			       default-directory)))

;;;###autoload
(defun emacs-aider-run-dwim ()
  "Start a new aider session for current project or the `default-directory'."
  (interactive)
  (let* ((buffer-name (emacs-aider--get-buffer-name))
	 (buffer (get-buffer-create buffer-name))
	 (final-args (emacs-aider--get-final-args)))
    (unless (process-live-p (get-buffer-process buffer))
      (emacs-aider--run buffer emacs-aider-command final-args))
    ;; restore default layout
    (delete-other-windows)
    (pop-to-buffer buffer emacs-aider-display-buffer-action t)
    (when emacs-aider-chat-window-selected-p
      (switch-to-buffer buffer))

    (with-current-buffer buffer
      ;; TODO
      )
    )
  )





(provide 'emacs-aider)
;;; emacs-aider.el ends here
