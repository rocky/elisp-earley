;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)


;; Load file to force the most recent read. And don't use bytecode.
(load-file "../earley-parser/objects.el")
(load-file "../earley-parser/tokens.el")
(load-file "../earley-parser/grammar-reader.el")

(test-simple-start)

;;-------------------------------------------------------------
(note "grammar lexicon reading")

(assert-equal
 (make-token :value "book"
	     :class "verb")
 (earley:parse-lexicon-line "book :class verb"))


(end-tests)
