;;; mrshpot-helpers.el --- Various stuff that doesn't deserve its own file

;; Filename: mrshpot-helpers.el
;; Description: Convenience functions
;; Author: Taras Shpot
;; Copyright (C) 2011-2015, Taras Shpot, all rights reserved.
;; Created: Jan 04 2011
;; Version: 0.1

;;; Code:


(defun insert-command-output (command)
  "Execute string COMMAND via \\[shell-command] and insert the output into the current buffer"
  (interactive
   ;; copied from \\[shell-command]
   (list (read-shell-command "Shell command: " nil nil
                             (let ((filename
                                    (cond
                                     (buffer-file-name)
                                     ((eq major-mode 'dired-mode)
                                      (dired-get-filename nil t)))))
                               (and filename (file-relative-name filename))))))
  (let ((shell-output nil))
    (with-temp-buffer
      (shell-command command (current-buffer))
      (setq shell-output (buffer-string)))
    (insert shell-output)))


(defun insert-c-header-guard-internal (guard)
  "Insert C header guard consisting of the ifdef/define/endif pattern."
  (save-excursion
    (goto-char (point-min))
    (insert "#ifndef " guard)
    (newline)
    (insert "#define " guard)
    (newline 2)
    (goto-char (point-max))
    (newline)
    (insert "#endif // " guard)
    (newline)))

(defun insert-c-header-guard-simple ()
  "Insert a C header guard in the form of FILE_NAME__INCLUDED"
  (interactive)
  (let ((guard
         (concat ""
                 (replace-regexp-in-string "\\.\\|-" "_" (buffer-name))
                 "__INCLUDED")))
    (insert-c-header-guard-internal (upcase guard))))

(defun insert-c-header-guard-uuid ()
  "Insert C header guard in the form of _FILE_NAME_UUID_"
  (interactive)
  (let* ((stripped-fname (replace-regexp-in-string "\\.[^.]*$" "" (buffer-name)))
         (guard-base 
          (concat stripped-fname "_" (shell-command-to-string "uuidgen")))
         (guard 
          (concat "_" (replace-regexp-in-string "\\.\\|-" "_" guard-base) "_")))
    (insert-c-header-guard-internal (upcase guard))))

;;; shell-n
(defun get-free-buffer-number (format-string)
  "Return the least N, for which buffer `(\\[format] FORMAT-STRING N) doesn't exist.
FORMAT-STRING will be passed to \\[format] and must contain a `%d' parameter."
  (interactive "sFormat-string (must contain %%d): ")
  (let ((i 1)
		(buffer-names (mapcar 'buffer-name (buffer-list))))
    (while (find (format format-string i) buffer-names :test 'string-equal)
      (setq i (1+ i)))
    i))

(setq shell-n-format "*shell-%d*")

(defun shell-n (&optional number)
  "Create a shell with name specified by `shell-n-format' (default \"*shell-%d*\")
Identical to \\[shell] with a prefix arg.
If NUMBER if provided, use it; otherwise use the first free number.

Query for NUMBER if a prefix arg present."
  (interactive
   (list (and current-prefix-arg
			  (read-number "Number: " (get-free-buffer-number shell-n-format)))))
  (let ((new-buffer-name
		 (format shell-n-format
				 (or number
					 (get-free-buffer-number shell-n-format)))))
    (shell (get-buffer-create new-buffer-name))))


(defun python-run-buffer-file (buffer-or-name)
  "Run buffer's file as a Python script.

Executes in comint buffer *python-run*."
  (interactive
   (list (if current-prefix-arg
			 (read-buffer "Python buffer: ")
		   (current-buffer))))
  (let*
	  ((script-buffer (get-buffer buffer-or-name))
	   (process-buffer (get-buffer-create "*python-run*"))
	   (script-filename (buffer-file-name script-buffer)))
	(make-comint-in-buffer
	 (buffer-name script-buffer)
	 process-buffer
	 (if (eq system-type 'windows-nt) "pythonw" "python")
	 nil
	 script-filename)
	(switch-to-buffer-other-window process-buffer)))
