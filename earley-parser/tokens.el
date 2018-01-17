;;;  -*- lexical-binding: t -*-
;;;; Representation of tokens or tokens or terminal symbols
;;;;-------------------------------------------------------

(require 'eieio)
(require 'cl-lib)
(require 'subr-x)
(require 'load-relative)

;; you may want to customize this.
;; However there should be a class field and a value field
(cl-defstruct token
  (class)
  (value))

(cl-defmethod format-token ((term token))
  (format "%s: %s" (token-value term) (token-class term)))

(cl-defmethod format-token-list ((l list))
  (concat "(" (mapconcat
     'format-token l  ", ") ")"))

(provide-me "earley-parser:")
