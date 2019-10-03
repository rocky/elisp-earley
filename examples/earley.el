(require 'cl-lib)
(require 'load-relative) ;; for __FILE__

;; Load file to force the most recent read. And don't use bytecode.
(load-file "../earley-parser/objects.el")
(load-file "../earley-parser/tokens.el")
(load-file "../earley-parser/grammar.el")

;;-------------------------------------------------------------
;; (defun lexicon-equal(a b)
;;   (and (equal (lexicon-token-dict a) (lexicon-token-dict b))
;;        (equal (lexicon-token-alphabet a) (lexicon-token-alphabet b))))

(setq my-dir (file-name-directory (__FILE__)))

(setq grammar-path (concat my-dir "../examples/elisp-full.g4"))
(setq my-grammar (earley:read-grammar-file grammar-path))

(assert-equal (grammar-productions "binary_op" my-grammar)
	      '(("EQLSIGN")
		("DIFF")))

(end-tests)
