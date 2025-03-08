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
  "whether selected the chat window when start a new aider session."
  :type 'boolean
  :group 'emacs-aider)

(defun emacs-aider--chat-buffer-p ()
  "Whether current buffer is `aider-chat' buffer."
  (string-match-p "^\\*emacs-aider.*\\*$" (buffer-name)))

(defun emacs-aider--run (buffer command cmd-args)
  "Start a new aider session."
  (apply #'make-comint-in-buffer "emacs-aider" buffer command nil cmd-args))

(defun emacs-aider--process-send-string (str)
  "Pre-process the `str' which send to `aider-chat' buffer."
  (let* ((str-tag (if (string-match-p "\n" str)
		      (format "{emacs-aider\n%s\nemacs-aider}" str)
		    str))
	 (str-trim (string-trim str-tag))
	 (str-newline (concat str-trim "\n")))
    (identity str-newline)))

(defun emacs-aider--send (buffer str)
  "Send command or contents to aider chat buffer."
  (let ((process (get-buffer-process buffer))
	(inhibit-read-only t))
    (with-current-buffer buffer
      (when (process-live-p process)
	(let ((process-str (emacs-aider--process-send-string str)))
	  (goto-char (process-mark process))
	  (insert process-str)
	  (set-marker (process-mark process) (point))
	  (comint-send-string process process-str))))))

(defun emacs-aider--get-buffer-name ()
  "Build `emacs-aider' buffer name according to `project-root' or `default-directory'."
  (format "*emacs-aider:%s*" (if-let* ((project (project-current)))
				 (project-root project)
			       default-directory)))
(defun emacs-aider--get-final-args ()
  "Build the final args of `emacs-aider-command'."
  ;; TODO
  ;; (format "%s" emacs-aider-command-default-args))
  (string-split emacs-aider-command-default-args nil))

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
      (setq comint-process-echoes t)
      (goto-char (point-max))
      ;; TODO
      )
    )
  )

;;;###autoload
(defalias #'emacs-aider-toggle-aider-chat #'emacs-aider-run-dwim)


;;;###autoload
(defun emacs-aider-query-region-or-defun (&optional capture)
  (interactive
   (list (cond ((region-active-p) (buffer-substring-no-properties (region-beginning)
								  (region-end)))
	       (t (thing-at-point 'defun)))))
  (let ((query-text (if capture
			(format "```\n%s\n```\n%s" capture (read-string "Aider Chat:"))
		      (read-string "Aider Chat:"))))
    (emacs-aider--send (emacs-aider--get-buffer-name) query-text)))

(provide 'emacs-aider)
;;; emacs-aider.el ends here
