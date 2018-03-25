;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(require 'cl-lib)


;; Load file to force the most recent read. And don't use bytecode.
(load-file "../earley-parser/objects.el")
(load-file "../earley-parser/tokens.el")
(load-file "../earley-parser/grammar.el")

(test-simple-start)

(declare-function make-token   'earley:tokens)
(declare-function token-value  'earley:tokens)
(declare-function make-grammar 'earley:objects)
(declare-function make-lexicon 'earley:objects)
(declare-function earley:read-next-grammar-token 'earley:grammar)
(declare-function earley:read-grammar-file       'earley:grammar)
(declare-function grammar-productions            'earley:grammar)
(declare-function grammar-rules-dict             'earley:grammar)

(eval-when-compile
  (defvar book-token)
  (defvar book-token-line)
  (defvar token-dict)
  (defvar grammar-buffer)
  (defvar my-grammar)
  (defvar my-dir)
  (defvar grammar-path))

;;-------------------------------------------------------------
;; (defun lexicon-equal(a b)
;;   (and (equal (lexicon-token-dict a) (lexicon-token-dict b))
;;        (equal (lexicon-token-alphabet a) (lexicon-token-alphabet b))))

(note "grammar lexicon reading")

(setq book-token (make-token :value "book" :class "verb"))

;; (assert-equal book-token
;; 	      (earley:parse-lexicon-line book-token-line))

(setq token-dict (make-hash-table :test 'equal))
(push book-token (gethash (token-value book-token) token-dict))

;; (lexicon-equal  # assert-equal
;;  (make-lexicon :token-dict token-dict :token-alphabet '("verb"))
;;  (earley:load-lexicon-from-string book-token-line))

(make-lexicon :token-dict token-dict :token-alphabet '("verb"))
;; (earley:load-lexicon-from-string book-token-line)

(defun read-grammar-token-test()
  (let ((temp-buffer (generate-new-buffer "*bnf-test*")))
    (with-current-buffer temp-buffer
      (insert "# This is a comment\n")
      (insert "<S>       ::= <Aux> <NP> <VP> | <VP>\n")
      (goto-char (point-min)))
    (assert-equal "S" (earley:read-next-grammar-token temp-buffer))
    (assert-equal "::=" (earley:read-next-grammar-token temp-buffer))
    (assert-equal "Aux" (earley:read-next-grammar-token temp-buffer))
    (assert-equal "NP" (earley:read-next-grammar-token temp-buffer))
    (assert-equal "VP" (earley:read-next-grammar-token temp-buffer))
    (assert-equal "|" (earley:read-next-grammar-token temp-buffer))
    (assert-equal "VP" (earley:read-next-grammar-token temp-buffer))
    (assert-equal nil (earley:read-next-grammar-token temp-buffer))
    (kill-buffer temp-buffer)))

(read-grammar-token-test)

(setq my-dir (file-name-directory (__FILE__)))

(setq grammar-path (concat my-dir "../examples/sentence.g4"))
(setq my-grammar (earley:read-grammar-file grammar-path))
(assert-t (grammar-rules-dict my-grammar))

(assert-equal (grammar-productions "S" my-grammar)
	      '(("NP" "VP")
		("VP")
		("Aux" "NP" "VP")))


(assert-equal (grammar-productions "NP" my-grammar)
	      '(("det" "nominal")
		("proper-noun")))

(assert-equal (grammar-productions "VP" my-grammar)
	      '(("verb")
		("verb" "NP")))

(assert-equal (grammar-productions "nominal" my-grammar)
	      '(("noun")
		("noun" "nominal")))

(setq grammar-path (concat my-dir "../examples/elisp.g4"))
(setq my-grammar (earley:read-grammar-file grammar-path))

(assert-equal (grammar-productions "binary_op" my-grammar)
	      '(("EQLSIGN")
		("DIFF")))

(end-tests)
