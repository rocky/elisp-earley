;;;  -*- lexical-binding: t -*-
;;;; Representation of tokens or terminal symbols
;;;;---------------------------------------------

(require 'eieio)
(require 'cl-lib)
(require 'subr-x)
(require 'load-relative)

;; you may want to customize this.
(cl-defstruct terminal
  (class)
  (gender)
  (word))

(cl-defmethod format-terminal ((term terminal))
  (format "%s: %s" (terminal-word term) (terminal-class term)))

(cl-defmethod format-terminal-list ((l list))
  (concat "(" (mapconcat
     'format-terminal l  ", ") ")"))

(provide-me "earley-parser:")
