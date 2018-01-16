;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)

;; Load file to force the most recent read. And don't use bytecode.
(load-file "./objects.el")
(load-file "./tokens.el")
(load-file "./earley-parser.el")

(test-simple-start)

;;-------------------------------------------------------------
(note "grammar rule creation")

;; 'rules' will contain our all of our grammar rules
(setq rules (make-hash-table :test 'equal))

;; Create a grammar with single rule and see that we can get that back.

(setq add_rhs '("expr1" "PLUS" "expr2") )
(push add_rhs (gethash "add" rules))
(setq g (make-grammar :rules rules))

(assert-equal (list add_rhs) (grammar-productions "add" g))

;; Add another rule for another nonterminal
(setq sub_rhs '("expr1" "MINUS" "expr2") )
(push sub_rhs (gethash "sub" rules))
(setq g (make-grammar :rules rules))

;; See that we have both grammar productions
(assert-equal (list add_rhs) (grammar-productions "add" g))
(assert-equal (list sub_rhs) (grammar-productions "sub" g))


;; Add another rule to the first nonterminal and see that that
;; is there too.

(setq add_rhs2 '("expr1") )
(push add_rhs2 (gethash "add" rules))
(setq g (make-grammar :rules rules))

(assert-equal (list add_rhs2 add_rhs) (grammar-productions "add" g))
(assert-equal (list sub_rhs) (grammar-productions "sub" g))

;;-------------------------------------------------------------
(note "Earley state tracking")

(setq s (make-state :condition "add"
 		    :subtree '("expr1" "PLUS" "expr2")
 		    :dot 2
 		    :constituent-index 1
 		    :dot-index 3))
(assert-equal "add -> expr1 PLUS . expr2 ; [1, 3]" (format-state s))
(assert-t (incomplete? s))
(assert-equal (follow-symbol s) "expr2")


(setq s (make-state :condition "add"
 		    :subtree '("expr1" "PLUS" "expr2")
 		    :dot 3
 		    :constituent-index 1
 		    :dot-index 3))

(assert-nil (follow-symbol s))

(assert-equal "add -> expr1 PLUS expr2 .  ; [1, 3]" (format-state s))
(assert-nil (incomplete? s))

;; ------------------------------------------------
(note "lexicon")

(setq rules (make-hash-table :test 'equal))

(setq dictionary (make-hash-table :test 'equal))
(setq part-of-speech nil)

(setq term (make-terminal :word "Test" :class "noun"))
(assert-equal "Test: noun" (format-terminal term))
(assert-equal "(Test: noun)" (format-terminal-list (list term)))

(pushnew (terminal-class term) part-of-speech :test 'equal)
(push term (gethash (terminal-word term) dictionary))

(setq lexicon (make-lexicon :dictionary dictionary :part-of-speech part-of-speech))

(assert-equal '("noun") (lexicon-part-of-speech lexicon))
(lexicon-lookup "Test" lexicon)

(setq c (make-chart))
(setq chart-listing (make-chart-listing))
(add-chart (make-chart) chart-listing)
(add-chart (make-chart) chart-listing)
(enqueue (make-state :condition "G"
		     :subtree (list "S")
		     :dot-index 0)
	 (nth 0 (chart-listing-charts chart-listing)))


(note "Earley parsing")

;; Create a grammar with single rule:
;;  S ::= noun

(setq rules (make-hash-table :test 'equal))
(setq start_rhs '("noun") )
(push start_rhs (gethash "S" rules))

(setq g (make-grammar :rules rules))

(setq p (earley-parse "" g lexicon))
(chart-listing->trees chart-listing)
(assert-t p)

;; Now parse with a Test which happens to be a noun:
(setq chart-listing (earley-parse "Test" g lexicon))
(assert-t chart-listing)

(print-chart-listing chart-listing)
(chart-listing->trees chart-listing)

(end-tests)
