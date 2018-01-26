;; More sophisicated parsing tests
;; Press C-x C-e at the end of the next line to run this file test non-interactively
;; (test-simple-run "emacs -batch -L %s -l %s" (file-name-directory (locate-library "test-simple.elc")) buffer-file-name)

(require 'test-simple)
(require 'load-relative)
(require 'cl-lib)

;; Load file to force the most recent read. And don't use bytecode.
(load-file "../earley-parser/objects.el")
(load-file "../earley-parser/tokens.el")
(load-file "../earley-parser/parser.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lots of compiler boilerplate to reduce warnings

(declare-function format-token 'earley:tokens)
(declare-function make-token   'earley:tokens)
(declare-function token-class  'earley:tokens)
(declare-function token-value  'earley:tokens)

(declare-function lexicon-lookup         'earley:objects)
(declare-function lexicon-token-alphabet 'earley:objects)
(declare-function make-grammar           'earley:objects)
(declare-function make-lexicon           'earley:objects)

(declare-function earley:parse 'earley:parser)

(declare-function earley:print-chart-listing  'earley:parser)
(declare-function earley:chart-listing->trees 'earley:parser)

(defvar chart-listing)
(defvar lexicon-dict)
(defvar my-grammar)
(defvar my-lexicon)
(defvar my-token)
(defvar number-token)
(defvar rules-dict)
(defvar token-alphabet)
(defvar token-dict)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test-simple-start)

(note "lexicon")
(setq token-alphabet nil)

;; token-dict is the set of tokens we can see. The key is a token
;; (class) name while the value is specific instance of that
;; token. In scanning the hash value is the value field of the
;; token, while the key is the token class name.
(setq token-dict (make-hash-table :test 'equal))

;; Add terminal symbols NUMBER with 1 and OP with +, -

(setq my-token (make-token :value "1" :class "NUMBER"))
(setq number-token my-token)
(assert-equal "1: NUMBER" (format-token my-token))

(pushnew (token-class my-token) token-alphabet :test 'equal)
(push my-token (gethash (token-value my-token) token-dict))

(setq my-token (make-token :value "+" :class "OP"))
(assert-equal "+: OP" (format-token my-token))
(assert-equal '("OP" "NUMBER") (pushnew (token-class my-token) token-alphabet :test 'equal))
(pushnew (token-class my-token) token-alphabet :test 'equal)
(push my-token (gethash (token-value my-token) token-dict))

(setq my-token (make-token :value "-" :class "OP"))
(pushnew (token-class my-token) token-alphabet :test 'equal)
(push my-token (gethash (token-value my-token) token-dict))

(setq my-lexicon
      (make-lexicon :token-dict token-dict :token-alphabet token-alphabet))

(assert-equal '("OP" "NUMBER") (lexicon-token-alphabet my-lexicon))
(assert-equal (list number-token) (lexicon-lookup "1" my-lexicon))

;; 'rules-dict' will contain our all of our grammar rules
;; the key is the LHS and the value is a list of RHS for that LHS
(setq rules-dict (make-hash-table :test 'equal))

;; Add grammar rules for:
;;   S ::= NUMBER | S OP NUMBER
(push '("NUMBER") (gethash "S" rules-dict))
(push '("S" "OP" "NUMBER") (gethash "S" rules-dict))
;; (push nil (gethash "S" rules-dict))

(setq my-grammar (make-grammar :rules-dict rules-dict :goal-symbol "S"))

(setq chart-listing (earley:parse "1" my-grammar my-lexicon))
(earley:print-chart-listing chart-listing)

(assert-equal
 '(("S"
    ("NUMBER" "1")))
 (earley:chart-listing->trees chart-listing)
 )

(setq chart-listing (earley:parse "" my-grammar my-lexicon))
(earley:print-chart-listing chart-listing)
(assert-equal nil (earley:chart-listing->trees chart-listing))

(setq chart-listing (earley:parse "1 + 1" my-grammar my-lexicon))
(earley:print-chart-listing chart-listing)
(assert-equal
 '(("S"
    ("S"
     ("NUMBER" "1"))
    ("OP" "+")
    ("NUMBER" "1")))
 (earley:chart-listing->trees chart-listing))

;; FIXME try a grammar with epsilon transitions
;;
;; (assert-equal '(("S" nil)) (earley:chart-listing->trees chart-listing))

;; FiXME: test that this fails
;; (setq char-listing (earley:parse "1 + 1 -" my-grammar my-lexicon))

;; FiXME: test that this words
;; (setq char-listing (earley:parse "1 + 1 - 1" my-grammar my-lexicon))


(end-tests)
