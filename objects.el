;;;  -*- lexical-binding: t -*-
;;; Objects used by the Earley parser

(require 'eieio)
(require 'cl)
(require 'subr-x)
(require 'load-relative)

(defvar *earley-debug* 3
  "Turns on parser debugging. 0 is no debugging. 4 is the
  maxiumum amount of debugging output" )

;;;; Representation of context-free grammar
;;;;---------------------------------------
(defstruct grammar
  (rules (make-hash-table :test 'equal)))

(cl-defmethod grammar-productions ((nonterminal string) (grammar grammar))
  "Returns the list of right-hand sides for a given nonterminal"
  (gethash nonterminal (grammar-rules grammar)))


;;;; Representation of lexicon
;;;;--------------------------
(defstruct lexicon
  (dictionary (make-hash-table :test 'equal))
  (part-of-speech nil))

(defun lexicon-lookup (word lexicon)
  (gethash word (lexicon-dictionary lexicon)))

;;;; representation of a parse state
;;;;--------------------------------

;; A parse state has embedded in it a grammar rule along with information
;; about how far the rule has progressed (the dot), its position
;; in the input sequence of tokens, and information to piece together
;; a parse tree (the parent state that caused this rule to get added).
(defstruct state
  ;; The left-hand nonterminal symbol of the rule of the state
  (lhs '? :type string)

  ;; The right hand side (= list) for the rule of the state
  (subtree)

  ;; indicates where the dot should be relative to this subtree
  ;; (what is _observed_ vs. what is _expected_).
  (dot 0)

  ;; index in sentence where the first in the sequence of
  ;; allowed successors should be.
  (constituent-index 0)

  ;; index relative to the sentence for where the dot should be.
  (dot-index 0)

  ;; When set, which previous state led to this state. This is
  ;; used for backtracing when creating a tree.
  (source-states nil))

(cl-defmethod format-state ((state state))
  (let ((lhs (state-lhs state))
        (subtree (state-subtree state))
        (dot (state-dot state)))
    (format "%s -> %s . %s ; (last token is %d)"
            lhs
	    (string-join (subseq subtree 0 dot) " ")
	    (string-join (subseq subtree dot (length subtree)) " ")
	    (state-dot-index state))))

(cl-defmethod incomplete? ((state state))
  "Returns whether or not there is anything left of the subtree behind the dot."
  (not (= (state-dot state) (length (state-subtree state)))))

(cl-defmethod follow-symbol ((state state))
  "Returns the following symbol (a nonterminal or terminal) 'state'"
  (let ((subtree (state-subtree state))
        (dot (state-dot state)))
    (when (> (length subtree) dot)
      (nth dot subtree))))

(cl-defmethod state->tree ((state state))
  "Creates a tree from a chart-listing object containting charts"
  (if (null (state-source-states state))
    (list (state-lhs state) (first (state-subtree state)))
    (cons (state-lhs state)
          (reverse (loop for state in (state-source-states state)
                         collect (state->tree state))))))


;;;; Representation of charts
;;;;-------------------------
(defstruct chart
  (states))

(defun enqueue (state chart)
  (if (cl-member state (chart-states chart) :test 'equal)
      (when (> *earley-debug* 3)
	(earley-msg (format "  the state %s is already in the chart" state)))
      (setf (chart-states chart) (append (chart-states chart) (list state)))))

;;;; Representation of chart listings
;;;;---------------------------------
(defstruct chart-listing
  ;; (start-symbol :type string)
  (charts))

(defun add-chart (chart chart-listing)
  (push chart (chart-listing-charts chart-listing)))

(defun print-chart-listing (chart-listing &optional final)
  (earley-msg "CHART-LISTING:")
  (loop for charts in (chart-listing-charts chart-listing)
     and index from 0
     do
     (earley-msg (format " %2d." index))
     (loop for state in (chart-states charts)
	   do (earley-msg (format "     %s" (format-state state))))))

(defun chart-listing->trees (chart-listing &optional start-symbol)
  "Return a list of trees created by following each successful parse in the last
 chart of 'chart-listings'"
  (unless start-symbol (setq start-symbol "S"))
  (loop for state in (chart-states
		      (first (last (chart-listing-charts chart-listing))))
     when (and (equal (state-lhs state) start-symbol)
	       (= (state-constituent-index state) 0)
	       (= (state-dot-index state)
		  (- (length (chart-listing-charts chart-listing)) 1))
	       (not (incomplete? state)))
     collect (state->tree state)))

(provide-me "earley-parser:")
