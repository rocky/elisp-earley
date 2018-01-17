;; More sophisicated parsing tests
;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)

;; Load file to force the most recent read. And don't use bytecode.
(load-file "./objects.el")
(load-file "./tokens.el")
(load-file "./earley-parser.el")

(test-simple-start)

(note "lexicon")
(setq part-of-speech nil)

(setq token-dict (make-hash-table :test 'equal))

;; Add terminal symbols NUMBER with 1 and OP with +, -

(setq token (make-terminal :word "1" :class "NUMBER"))
(setq number-token token)
(assert-equal "1: NUMBER" (format-terminal token))

(pushnew (terminal-class token) part-of-speech :test 'equal)
(push token (gethash (terminal-word token) token-dict))

(setq token (make-terminal :word "+" :class "OP"))
(assert-equal "+: OP" (format-terminal token))
(assert-equal '("OP" "NUMBER") (pushnew (terminal-class token) part-of-speech :test 'equal))
(pushnew (terminal-class token) part-of-speech :test 'equal)
(push token (gethash (terminal-word token) token-dict))

(setq token (make-terminal :word "-" :class "OP"))
(pushnew (terminal-class token) part-of-speech :test 'equal)
(push token (gethash (terminal-word token) token-dict))

(setq lexicon (make-lexicon :dictionary token-dict :part-of-speech part-of-speech))

(assert-equal '("OP" "NUMBER") (lexicon-part-of-speech lexicon))
(assert-equal (list number-token) (lexicon-lookup "1" lexicon))

;; Add grammar rules
;;   S ::= NUMBER | S OP NUMBER |
(setq rules (make-hash-table :test 'equal))

(push '("NUMBER") (gethash "S" rules))
(push '("S" "OP" "NUMBER") (gethash "S" rules))
(push nil (gethash "S" rules))

(setq grammar (make-grammar :rules rules :start-symbol "S"))

(setq chart-listing (earley-parse "1" grammar lexicon))
(print-chart-listing chart-listing)

(assert-equal
 '(("S"
    ("NUMBER" "1")))
 (chart-listing->trees chart-listing)
 )

(setq chart-listing (earley-parse "" grammar lexicon))
(print-chart-listing chart-listing)
(assert-equal '(("S" nil)) (chart-listing->trees chart-listing))

(setq chart-listing (earley-parse "1 + 1" grammar lexicon))
(print-chart-listing chart-listing)
(assert-equal
 '(("S"
    ("S"
     ("NUMBER" "1"))
    ("OP" "+")
    ("NUMBER" "1")))
 (chart-listing->trees chart-listing))

(end-tests)
