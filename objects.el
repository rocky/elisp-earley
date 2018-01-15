;;;  -*- lexical-binding: t -*-
;;; Objects used by the Earley parser

(require 'eieio)
(require 'cl)
(require 'load-relative)

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

(defmethod lexicon-lookup ((word string) (lexicon lexicon))
  (gethash word (lexicon-dictionary lexicon)))

;;;; representation of state
;;;;------------------------
(defstruct state
  ;; the "head" of the state
  (condition '? :type string)
  ;; a tree (= list) representing an allowed sequence of successors for the
  ;; condition.
  (subtree)
  ;; indicates where the dot should be relative to this subtree
  ;; (whats is _observed_ vs what is _expected_).
  (dot 0)
  ;; index in sentence where the first in the sequence of
  ;; allowed successors should be.
  (constituent-index 0)
  ;; index relative to the sentence for where the dot should be.
  (dot-index 0)
  ;; find out which previous state led to this state. This is
  ;; used for backtracing when creating a tree.
  (source-states))

(defmethod format-state ((state state))
  (let ((condition (state-condition state))
        (subtree (state-subtree state))
        (dot (state-dot state)))
    (format "%s -> %s . %s ; [%s, %s]"
            condition
	    (string-join (subseq subtree 0 dot) " ")
	    (string-join (subseq subtree dot (length subtree)) " ")
            (state-constituent-index state)
	    (state-dot-index state))))

(defmethod incomplete? ((state state))
  "Returns whether or not there is anything left of the subtree behind the dot."
  (not (= (state-dot state) (length (state-subtree state)))))

(defmethod next-cat ((state state))
  "Returns the next category of 'state'"
  (let ((subtree (state-subtree state))
        (dot (state-dot state)))
    (when (> (length subtree) dot)
      (nth dot subtree))))

(defmethod state->tree ((state state))
  "Creates a tree from a chart-listing object containting charts"
  (if (null (state-source-states state))
    (list (state-condition state) (first (state-subtree state)))
    (cons (state-condition state)
          (reverse (loop for state in (state-source-states state)
                         collect (state->tree state))))))


;;;; Representation of charts
;;;;-------------------------
(defstruct chart
  (states))

(defmethod enqueue ((state state) (chart chart))
  (if (cl-member state (chart-states chart) :test *string-comparer*)
      (when (> *earley-debug* 3)
	(message "  the state %s is already in the chart" state))
      (setf (chart-states chart) (append (chart-states chart) (list state)))))

(defmethod print-object ((chart chart)(s stream))
  (message "#CHART:")
  (loop for state in (chart-states chart)
     do (message "    %s" state)))


;;;; Representation of chart listings
;;;;---------------------------------
(defstruct chart-listing
  (charts))

(defmethod add-chart ((chart chart) (chart-listing chart-listing))
  (push chart (chart-listing-charts chart-listing)))

(defmethod print-object ((chart-listing chart-listing)(s stream))
  (format s "#CHART-LISTING:")
  (loop for chart in (chart-listing-charts chart-listing)
     and index from 0
     do (message "  %s. %s" index chart)))

(defmethod chart-listing->trees ((chart-listing chart-listing))
  "Return a list of trees created by following each successful parse in the last
 chart of 'chart-listings'"
  (loop for state in (chart-states
		      (first (last (chart-listing-charts chart-listing))))
     when (and (funcall *string-comparer* (state-condition state) "S")
	       (= (state-constituent-index state) 0)
	       (= (state-dot-index state)
		  (- (length (chart-listing-charts chart-listing)) 1))
	       (not (incomplete? state)))
     collect (state->tree state)))

(provide-me "earley-parser:")
