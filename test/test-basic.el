;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)

;; Load file to force the most recent read. And don't use bytecode.
(load-file "../earley-parser/objects.el")
(load-file "../earley-parser/tokens.el")
(load-file "../earley-parser/parser.el")

(test-simple-start)

;;-------------------------------------------------------------
(note "grammar rule creation")

;; 'rules' will contain our all of our grammar rules
(setq rules (make-hash-table :test 'equal))

;; Create a grammar with single rule and see that we can get that back.

(setq rhs '("expr1" "PLUS" "expr2") )
(push rhs (gethash "add" rules))
(setq grammar (make-grammar :rules rules))

(assert-equal (list rhs) (grammar-productions "add" grammar))

;; Add another rule for another nonterminal
(setq sub_rhs '("expr1" "MINUS" "expr2") )
(push sub_rhs (gethash "sub" rules))
(setq grammar (make-grammar :rules rules))

;; See that we have both grammar productions
(assert-equal (list rhs) (grammar-productions "add" grammar))
(assert-equal (list sub_rhs) (grammar-productions "sub" grammar))


;; Add another rule to the first nonterminal and see that that
;; is there too.

(setq rhs2 '("expr1") )
(push rhs2 (gethash "add" rules))
(setq grammar (make-grammar :rules rules))

(assert-equal (list rhs2 rhs) (grammar-productions "add" grammar))
(assert-equal (list sub_rhs) (grammar-productions "sub" grammar))

;;-------------------------------------------------------------
(note "Earley state tracking")

(setq s (make-state :lhs "add"
 		    :rhs '("expr1" "PLUS" "expr2")
 		    :dot 2
 		    :constituent-index 1
 		    :dot-index 3))
(assert-equal "add -> expr1 PLUS . expr2 ; (last token is 3)" (format-state s))
(assert-t (incomplete? s))
(assert-equal (follow-symbol s) "expr2")


(setq s (make-state :lhs "add"
 		    :rhs '("expr1" "PLUS" "expr2")
 		    :dot 3
 		    :constituent-index 1
 		    :dot-index 3))

(assert-nil (follow-symbol s))

(assert-equal "add -> expr1 PLUS expr2 .  ; (last token is 3)" (format-state s))
(assert-nil (incomplete? s))

;; ------------------------------------------------
(note "lexicon")

(setq part-of-speech nil)

(setq token-dict (make-hash-table :test 'equal))

(setq term (make-terminal :word "Test" :class "noun"))
(assert-equal "Test: noun" (format-terminal term))
(assert-equal "(Test: noun)" (format-terminal-list (list term)))

(pushnew (terminal-class term) part-of-speech :test 'equal)
(push term (gethash (terminal-word term) token-dict))

(setq lexicon (make-lexicon :dictionary token-dict :part-of-speech part-of-speech))

(assert-equal '("noun") (lexicon-part-of-speech lexicon))
(lexicon-lookup "Test" lexicon)

(setq c (make-chart))
(setq chart-listing (make-chart-listing))
(add-chart (make-chart) chart-listing)
(add-chart (make-chart) chart-listing)
(enqueue (make-state :lhs "G"
		     :rhs (list "S")
		     :dot-index 0)
	 (nth 0 (chart-listing-charts chart-listing)))


(note "Earley parsing")

;; Create a grammar with single rule:
;;  S ::= noun

(setq rules (make-hash-table :test 'equal))
(setq rhs '("noun") )
(push rhs (gethash "S" rules))
(setq grammar (make-grammar :rules rules :start-symbol "S"))

;; Parse the simplest possible thing, null input.
;; There are no prediction, scanning, or reduction routines
;; then get called for this
(setq chart-listing (earley-parse "" grammar lexicon))
(assert-equal
 (make-chart-listing
  :start-symbol "S"
  :charts
  (list (make-chart
	 :states (list (make-state :lhs "G" :rhs '("S"))
		       (make-state :lhs "S" :rhs '("noun"))))))
 chart-listing
 "Null sentence parse states")
(print-chart-listing chart-listing)
(assert-nil (chart-listing->trees chart-listing)
	    "Null sentence should produce nil (no tree)")

;; Now parse with sentence "Test" which happens to be a noun:
(setq chart-listing (earley-parse "Test" grammar lexicon))
(assert-t chart-listing)

(print-chart-listing chart-listing)
(setq result (chart-listing->trees chart-listing))
(assert-equal result '(("S" ("noun" "Test")))
	      "Correct parse of single derivation")

(end-tests)
