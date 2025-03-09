;;; emacs-aider.el --- Emacs integration for aider.chat -*- lexical-binding: t -*-

;; Copyright (C) 2025 xplutoyz

;; Author: xplutoyz
;; Created: 2025-03-08 13:10:34
;; Version: 0.1
;; Package-Requires: ((emacs "30.1"))

;;; Commentary:
;; Emacs integration for aider.chat - an AI pair programming assistant

;;; Code:

(require 'comint)
(require 'project)

(defgroup emacs-aider nil
  "Emacs integration for aider.chat"
  :group 'tools)

(defcustom emacs-aider-command "aider"
  "The command to run the aider chat interface."
  :type 'string
  :group 'emacs-aider)

(defcustom emacs-aider-command-default-args '("--model" "deepseek")
  "The command args for `emacs-aider-command'."
  :type 'list
  :group 'emacs-aider)

(defcustom emacs-aider-display-buffer-action nil
  "The default display action of `emacs-aider' buffer."
  :type 'list
  :group 'emacs-aider)

(defcustom emacs-aider-prefix-key "C-c p"
  "The prefix key for emacs-aider commands.
This should be a string that can be passed to `kbd'."
  :type 'string
  :group 'emacs-aider)

(defun emacs-aider--chat-buffer-p ()
  "Whether current buffer is `aider-chat' buffer."
  (string-match-p "^\\*emacs-aider.*\\*$" (buffer-name)))

(defun emacs-aider--allow-to-run-p ()
  "Whether allowed to start aider chat session."
  (not (emacs-aider--chat-buffer-p)))

(defun emacs-aider--run (buffer command cmd-args)
  "Start a new aider session."
  (apply #'make-comint-in-buffer "emacs-aider" buffer command nil cmd-args))

(defun emacs-aider--process-send-string (str)
  "Pre-process the `str' which send to `aider-chat' buffer."
  (let* ((str-tag
          (if (string-match-p "\n" str)
              (format "{aider\n%s\naider}" str)
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

(defun emacs-aider--root-dir ()
  "Current `project-root' or `default-directory'."
  (if-let* ((prj (project-current)))
    (project-root prj)
    default-directory))

(defun emacs-aider--get-buffer-name ()
  "Build `emacs-aider' buffer name according to `emacs-aider--root-dir'."
  (format "*emacs-aider:%s*" (emacs-aider--root-dir)))

(defun emacs-aider--send-command-on-file (operand)
  "Send command whose `operand' is filename."
  (let* ((file-name (expand-file-name (read-file-name "Choose file: " (buffer-file-name))))
         (command (format "%s %s" operand file-name)))
    (when (emacs-aider-run-dwim)
      (emacs-aider--send (emacs-aider--get-buffer-name) command))))

(defun emacs-aider--chat-alive-p ()
  "Wether the aider session is alive."
  (process-live-p (get-buffer-process (emacs-aider--get-buffer-name))))

;;;###autoload
(defun emacs-aider-other-operator ()
  "Send other unin operator."
  (interactive)
  (if (emacs-aider--chat-alive-p)
      (emacs-aider--send
       (emacs-aider--get-buffer-name)
       (completing-read "Choose an operate:" '("/reset" "/undo" "/paste" "/clear" "/help" "/ls")))
    (message "There is no alive aider chat session.")))

;;;###autoload
(defun emacs-aider-quit ()
  "Quit aider chat session."
  (interactive)
  (when-let* ((buffer (get-buffer (emacs-aider--get-buffer-name))))
    (when (process-live-p (get-buffer-process buffer))
      (emacs-aider--send buffer "\quit"))
    (delete-windows-on buffer)
    (kill-buffer buffer)))

;;;###autoload
(defun emacs-aider-switch-chat-mode ()
  "Switch to other aider chat mode."
  (interactive)
  (when (emacs-aider-run-dwim)
    (emacs-aider--send
     (emacs-aider--get-buffer-name)
     (format "%s" (completing-read "Choose a mode" '("/ask" "/code" "/architect"))))))

;;;###autoload
(defun emacs-aider-add-web-contents ()
  "Add web contents to aider chat."
  (interactive)
  (when (emacs-aider-run-dwim)
    (emacs-aider--send (emacs-aider--get-buffer-name) (format "/web %s" (read-string "url:")))))

;;;###autoload
(defun emacs-aider-add-file-by-dired-marked (&optional read-only)
  "Add files to adier session from dired."
  (interactive "P")
  (when (eq major-mode 'dired-mode)
    (let* ((marked-files (dired-get-marked-files))
           (joined-files (string-join marked-files " "))
           (command
            (format "%s %s"
                    (if read-only
                        "/read-only"
                      "/add")
                    joined-files)))
      (dired-unmark-all-marks)
      (when (emacs-aider-run-dwim)
        (emacs-aider--send (emacs-aider--get-buffer-name) command)))))

;;;###autoload
(defun emacs-aider-add-single-file (&optional read-only)
  "Add a single file to aider chat."
  (interactive "P")
  (emacs-aider--send-command-on-file
   (if read-only
       "/read-only"
     "/add")))

;;;###autoload
(defun emacs-aider-drop-single-file ()
  "Drop a single file from aider chat."
  (interactive)
  (emacs-aider--send-command-on-file "/drop"))

;;;###autoload
(defun emacs-aider-run-dwim ()
  "Start a new aider session for current project or the `default-directory'."
  (interactive)
  (if (emacs-aider--allow-to-run-p)
      (let* ((buffer-name (emacs-aider--get-buffer-name))
             (buffer (get-buffer-create buffer-name)))
        (unless (process-live-p (get-buffer-process buffer))
          (emacs-aider--run buffer emacs-aider-command emacs-aider-command-default-args))
        ;; restore default layout
        (delete-other-windows)
        (display-buffer buffer emacs-aider-display-buffer-action t)
        (with-current-buffer buffer
          (goto-char (point-max))
          (setq comint-process-echoes t)))
    (message "It is not allowed to run here")
    nil))

;;;###autoload
(defun emacs-aider-query-dwim (&optional capture)
  (interactive (list
                (cond
                 ((region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end)))
                 (t
                  (thing-at-point 'defun)))))
  (let ((user-input (read-string "Aider Chat: "))
        (query-text
         (if capture
             (format "\n```\n%s\n```\n%s" capture user-input)
           user-input)))
    (when (emacs-aider-run-dwim)
      (emacs-aider--send (emacs-aider--get-buffer-name) query-text))))


(defvar global-emacs-aider-mode-map
  (let ((map (make-sparse-keymap)))
    (keymap-set map (concat emacs-aider-prefix-key " r") #'emacs-aider-run-dwim)
    (keymap-set map (concat emacs-aider-prefix-key " q") #'emacs-aider-quit)
    (keymap-set map (concat emacs-aider-prefix-key " a") #'emacs-aider-add-single-file)
    (keymap-set map (concat emacs-aider-prefix-key " A") #'emacs-aider-add-file-by-dired-marked)
    (keymap-set map (concat emacs-aider-prefix-key " d") #'emacs-aider-drop-single-file)
    (keymap-set map (concat emacs-aider-prefix-key " w") #'emacs-aider-add-web-contents)
    (keymap-set map (concat emacs-aider-prefix-key " s") #'emacs-aider-switch-chat-mode)
    (keymap-set map (concat emacs-aider-prefix-key " o") #'emacs-aider-other-operator)
    (keymap-set map (concat emacs-aider-prefix-key " RET") #'emacs-aider-query-dwim)
    map)
  "Keymap for `global-emacs-aider-mode'.")

;;;###autoload
(define-minor-mode global-emacs-aider-mode
  "Global minor mode for emacs-aider.
When enabled, provides key bindings for interacting with the aider chat interface.

Key bindings are defined under the prefix `emacs-aider-prefix-key' (default \"C-c p\").

\\{global-emacs-aider-mode-map}"
  :global t
  :keymap global-emacs-aider-mode-map
  :group 'emacs-aider
  :lighter " EAider")

(provide 'emacs-aider)
;;; emacs-aider.el ends here
