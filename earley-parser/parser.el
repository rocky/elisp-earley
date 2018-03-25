;;;  -*- lexical-binding: t -*-

;; A parser takes three inputs: a grammar, a lexicon (perhaps defined
;; with regular expressions), and a tokens to parse.  This parser then
;; returns a parse tree.
;;
;; Earley Parsers are often also called "Chart parsers", because every
;; time the next token in a sentence is read, that a new chart is
;; created.  Each chart contains a set of "states" that roughly
;; correspond to rules in the specified grammar that are in different
;; states, depending upon what input has already been seen.

;; The Earley Parser algorithm starts with exactly one chart, with the
;; starting state. With this it "predicts" the next states. Through
;; "prediction", "scanning" and reduction more states get added to
;; this initial chart.  At some point a token is encountered which
;; triggers a scan, which in turn causes the chart-index to be
;; incremented, and the next set of states that are created are added
;; to a new chart.  This continues, where new charts are created and
;; states are added to each of those charts.  A list of charts is just
;; a list of a list of states.

(require 'eieio)
(require 'cl-lib)
(require 'load-relative)
(load-relative "./objects")
(load-relative "./tokens")
(load-relative "./msg")

(defvar earley:debug)

(defun predictor (state chart-listing grammar)
  "Predict possible successor states based on the grammar. As a side-effect, add
 these states to the chart that this state belong to."
  (let ((B (follow-symbol state))
        (j (state-dot-index state)))
    (loop for production in (grammar-productions B grammar)
       collect (let ((new-state
		      (make-state :lhs B
				  :rhs production
				  :dot 0
				  :constituent-index j
				  :dot-index j)))
		 (when (> earley:debug 2)
		   (earley:msg (format "  shifting new state into chart %d\n\t%s"
				       j (format-state new-state))))

		 (earley:enqueue new-state (nth j (chart-listing-charts
						   chart-listing)))
		 new-state))))

(defun follow-match?(follow-symbol token)
    (equal follow-symbol (token-class token)))

(defun token?(symbol lexicon)
  (cl-member symbol (lexicon-token-alphabet lexicon)
	     :test 'equal))

(cl-defmethod scanner ((state state)
		    (tokens list)
		    (chart-listing chart-listing)
		    (lexicon lexicon))
  "Check if the next symbol of `state' is a member of the
 post-categories for the current token. As a side effect, queue
 a new state corresponding to th is find, into the current
 chart."
  (let* ((follow (follow-symbol state))
         (j (state-dot-index state))
         (token (nth j tokens)))
    (when (> earley:debug 2)
      (earley:msg
       (format "  scanner checking if \"%s\" is in %s"
	       follow
	       (format-token-list (token-lookup token lexicon)))))
    (when (cl-member follow (token-lookup token lexicon)
		  :test 'follow-match?)
      (let ((new-state (make-state :lhs follow
				   :rhs (list token)
				   :dot 1
				   :constituent-index j
				   :dot-index (+ j 1))))
        (when (> earley:debug 2)
	  (earley:msg
	   (format "  scanner enqueuing into chart %d if new\n\t%s"
		   (+ j 1) (format-state new-state))))
        (earley:enqueue
	 new-state (nth (+ j 1) (chart-listing-charts chart-listing)))))))

(defun completer (state chart-listing)
  "Find and return a list of the previous states that expect this
state's symbol at this dot-index with the dot moved one step
forward. As a side-effect, queue the states in the current
chart. Returns the chart-list with contains the completed states
added to the chart-list."
  (let ((B (state-lhs state))
        (j (state-constituent-index state))
        (k (state-dot-index state)))
    (loop for prev-state
       in (chart-states (nth j (chart-listing-charts chart-listing)))
       when (let ((A (follow-symbol prev-state)))
	      (when (equal A B)
		(when (> earley:debug 3)
		  (earley:msg (format "  completer afound match for %s, state:\n\t%s"
				      state (format-state prev-state))))
		t))
       collect (let ((new-state (make-state
				 :lhs (state-lhs prev-state)
				 :rhs (state-rhs prev-state)
				 :dot (+ (state-dot prev-state) 1)
				 :constituent-index (state-constituent-index
						     prev-state)
				 :dot-index k
				 :source-states (append (list state)
							(state-source-states
							 prev-state)))))
		 (when (> earley:debug 2)
		   (earley:msg
		    (format "  reduction into chart %d if new:\n\t%s"
			    k (format-state new-state))))
		 (earley:enqueue
		  new-state (nth k (chart-listing-charts chart-listing)))
		 new-state))))

(defun earley:parse (sentence grammar lexicon)
  "Parse the input string of tokens into a chart conforming to the grammar.
   Afterwards you can check to see if the parse covered the entire string."
  (let* ((tokens (remove "" (split-string sentence "[ \t\n]+")))
	 (goal-symbol (grammar-goal-symbol grammar))
	 (chart-listing (make-chart-listing :goal-symbol goal-symbol)))

    (assert (stringp goal-symbol) nil
		     "Grammar should have a nonnil goal-symbol set")

    (earley:msg-clear)
    ;; (setf (chart-listing-goal-symbol chart-listing) goal-symbol)
    ;; Initialize charts, one chart per token in the sentence
    (loop for i from 0 to (length tokens)
       do (earley:add-chart (make-chart) chart-listing))

    ;; FIXME check that G is not in grammar but goal-symbol is!

    ;; Start off by enqueuing a dummy state in the first chart
    (earley:enqueue (make-state :lhs "G"
				:rhs (list goal-symbol)
				:dot-index 0)
		    (nth 0 (chart-listing-charts chart-listing)))

    ;; Then for each chart (= one per token)...
    (loop for chart in (chart-listing-charts chart-listing)
       and chart-index from 0
       ;; And for each possible state in that chart
       do (progn
	    (when (> earley:debug 0)
	      (earley:msg
	       (format "---- Processing Chart %s ----" chart-index)))
	    (loop for state-index from 0
	       until (>= state-index (length (chart-states chart)))
	       do (let ((state (nth state-index (chart-states chart))))
		    (when (> earley:debug 1)
		      (earley:msg
		       (format "Considering%s rule:\n\t%s"
			       (if (incomplete? state) " unfinished" "")
			       (format-state state))))
		    (cond ((and (incomplete? state)
				(not (token? (follow-symbol state) lexicon)))
			   (when (> earley:debug 1)
			     (earley:msg "Predicting..."))
			   (predictor state chart-listing grammar))
			  ((and (incomplete? state)
				(token? (follow-symbol state) lexicon))
			   (unless (eq chart (first
					      (last
					       (chart-listing-charts
						chart-listing))))
			     (when (> earley:debug 1)
			       (earley:msg "Scanning..."))
			     (scanner state tokens chart-listing lexicon)))
			  (t
			   (when (> earley:debug 1)
			     (earley:msg "Reductions..."))
			   (completer state chart-listing)))))
	    (when (> earley:debug 0)
	      (earley:msg ""))))
    (earley:msg "==============================================")
    chart-listing))

(provide-me "earley-parser:")
