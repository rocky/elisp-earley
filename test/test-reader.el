;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)


;; Load file to force the most recent read. And don't use bytecode.
(load-file "../earley-parser/objects.el")
(load-file "../earley-parser/tokens.el")
(load-file "../earley-parser/grammar-reader.el")

(test-simple-start)

(defvar book-token)
(defvar book-token-line)
(defvar token-dict)

;;-------------------------------------------------------------
;; (defun lexicon-equal(a b)
;;   (and (equal (lexicon-token-dict a) (lexicon-token-dict b))
;;        (equal (lexicon-token-alphabet a) (lexicon-token-alphabet b))))

(note "grammar lexicon reading")

(setq book-token (make-token :value "book" :class "verb"))

(assert-equal book-token
	      (earley:parse-lexicon-line book-token-line))

(setq token-dict (make-hash-table :test 'equal))
(push book-token (gethash (token-value book-token) token-dict))

;; (lexicon-equal  # assert-equal
;;  (make-lexicon :token-dict token-dict :token-alphabet '("verb"))
;;  (earley:load-lexicon-from-string book-token-line))

(make-lexicon :token-dict token-dict :token-alphabet '("verb"))
(earley:load-lexicon-from-string book-token-line)

(end-tests)
