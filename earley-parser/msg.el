;;;  -*- lexical-binding: t -*-
;;; I/O routines and Messaging system
(require 'load-relative)

(defvar earley-debug-buffer "*Earley-Debug-Log*"
  "Name of buffer to put parser debug messages"
  )

(defun earley:msg-clear()
  "Initialize and reset everything before parsing"

  (interactive)

  (with-current-buffer (get-buffer-create earley-debug-buffer)
    (let ((old-read-only inhibit-read-only))
      (setq inhibit-read-only 't)
      (delete-region (point-min) (point-max))
      (setq inhibit-read-only old-read-only)))
  (unless noninteractive
    (message "Earley debug log initialized")))

(defun earley:msg(msg &optional no-newline)
  (switch-to-buffer earley-debug-buffer)
  (let ((inhibit-read-only t))
    (insert msg)
    (unless no-newline (insert "\n"))
    (switch-to-buffer nil)
  ))

(provide-me "earley-parser:")
