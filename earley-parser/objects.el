;;;  -*- lexical-binding: t -*-
;;; Objects used by the Earley parser

;; cl is used instead of cl-lib to handle "loop" construct
(with-no-warnings
   (require 'cl))

(require 'cl-lib)
(require 'eieio)
(require 'subr-x)
(require 'load-relative)

(load-relative "./msg")

(declare-function earley-msg 'early-parser:msg)

;; FIXME turn into a defcustom
(defvar *earley-debug* 3
  "Turns on parser debugging. 0 is no debugging. 4 is the
  maxiumum amount of debugging output" )

;;;; Representation of context-free grammar
;;;;---------------------------------------
(cl-defstruct grammar
  (start-symbol nil :type string)

  ;; 'rules-dict' contains grammar rules as a hash table indexed by
  ;; LHS nonterminals.  The key is the left-hand side of the rule and
  ;; the value is a list of right-hand-sides for the nonterminal.

  ;; For example for the grammar:
  ;;    S ::= NUMBER | S OP NUMBER |

  ;; the rules-dict would be:

  ;; #s(hash-table ...
  ;; ("S"
  ;;  (nil
  ;;   ("S" "OP" "NUMBER")
  ;;   ("NUMBER"))))
  ;;
  ;; Note the use of the nil list to represent an epsilon transition

  (rules-dict (make-hash-table :test 'equal)))

(cl-defmethod grammar-productions ((nonterminal string) (grammar grammar))
  "Returns the list of right-hand sides for a given nonterminal"
  (gethash nonterminal (grammar-rules-dict grammar)))


;;;; Representation of lexicon
;;;;--------------------------
(cl-defstruct lexicon
  (dictionary (make-hash-table :test 'equal))
  (part-of-speech nil))

(defun lexicon-lookup (token lexicon)
  (gethash token (lexicon-dictionary lexicon)))

;;;; representation of a parse state
;;;;--------------------------------

;; A parse state has embedded in it a grammar rule along with information
;; about how far the rule has progressed (the dot), its position
;; in the input sequence of tokens, and information to piece together
;; a parse tree (the parent state that caused this rule to get added).
(cl-defstruct state
  ;; The left-hand nonterminal symbol of the grammar rule of the state
  (lhs '? :type string)

  ;; The right-hand side of the grammar rule of the state
  (rhs nil :type list)

  ;; indicates where the dot should be relative to the rule's rhs
  ;; (what is _observed_ vs. what is _expected_).
  (dot 0 :type integerp)

  ;; index in sentence where the first in the sequence of
  ;; allowed successors should be.
  (constituent-index 0 :type integer p)

  ;; index relative to the sentence for where the dot should be.
  (dot-index 0 :integerp)

  ;; When set, which previous state led to this state. This is
  ;; used for backtracing when creating a tree.
  (source-states nil))

(cl-defmethod format-state ((state state))
  (let ((lhs (state-lhs state))
        (rhs (state-rhs state))
        (dot (state-dot state)))
    (format "%s -> %s . %s ; (last token is %d)"
            lhs
	    (string-join (cl-subseq rhs 0 dot) " ")
	    (string-join (cl-subseq rhs dot (length rhs)) " ")
	    (state-dot-index state))))

(cl-defmethod incomplete? ((state state))
  "Returns whether or not there is anything left of the rhs behind the dot."
  (not (= (state-dot state) (length (state-rhs state)))))

(cl-defmethod follow-symbol ((state state))
  "Returns the following symbol (a nonterminal or terminal) 'state'"
  (let ((rhs (state-rhs state))
        (dot (state-dot state)))
    (when (> (length rhs) dot)
      (nth dot rhs))))

(cl-defmethod state->tree ((state state))
  "Creates a tree from a chart-listing object containting charts"
  (if (null (state-source-states state))
    (list (state-lhs state) (first (state-rhs state)))
    (cons (state-lhs state)
          (reverse (loop for state in (state-source-states state)
                         collect (state->tree state))))))


;;;; Representation of charts
;;;;-------------------------
(cl-defstruct chart
  (states nil :type list))

(defun enqueue (state chart)
  (if (cl-member state (chart-states chart) :test 'equal)
      (when (> *earley-debug* 3)
	(earley-msg (format "  the state %s is already in the chart" state)))
      (setf (chart-states chart) (append (chart-states chart) (list state)))))

;;;; Representation of chart listings
;;;;---------------------------------
(cl-defstruct chart-listing
  (start-symbol nil :type string)
  (charts nil :type list))

(cl-defmethod add-chart ((chart chart) (chart-listing chart-listing))
  (push chart (chart-listing-charts chart-listing)))

(cl-defmethod print-chart-listing ((chart-listing chart-listing))
  (earley-msg "CHART-LISTING:")
  (loop for charts in (chart-listing-charts chart-listing)
     and index from 0
     do
     (earley-msg (format " %2d." index))
     (loop for state in (chart-states charts)
	   do (earley-msg (format "     %s" (format-state state))))))

(cl-defmethod chart-listing->trees ((chart-listing chart-listing))
  "Return a list of trees created by following each successful parse in the last
 chart of 'chart-listings'"
  (let ((start-symbol (chart-listing-start-symbol chart-listing)))
    (loop for state in (chart-states
			(first (last (chart-listing-charts chart-listing))))
	  when (and (equal (state-lhs state) start-symbol)
		    (= (state-constituent-index state) 0)
		    (= (state-dot-index state)
		       (- (length (chart-listing-charts chart-listing)) 1))
		    (not (incomplete? state)))
	  collect (state->tree state))))

(provide-me "earley-parser:")
