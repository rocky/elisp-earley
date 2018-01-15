;;;; Representation of terminals -
;;;;----------------------------

(require 'load-relative)

;; you may want to customize this.
(defstruct terminal
  (class)
  (gender)
  (word))

(provide-me "earley-parser:")
