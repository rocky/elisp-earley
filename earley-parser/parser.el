;;;  -*- lexical-binding: t -*-

(require 'eieio)
(require 'cl-lib)
(require 'load-relative)
(load-relative "./objects")
(load-relative "./tokens")
(load-relative "./msg")

(defvar *earley-debug*)

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
		 (when (> *earley-debug* 2)
		   (earley-msg (format "  shifting new state into chart %d\n\t%s"
				       j (format-state new-state))))

		 (enqueue new-state (nth j (chart-listing-charts
					    chart-listing)))
		 new-state))))

(defun follow-match?(follow-symbol token)
    (equal follow-symbol (token-class token)))

(cl-defmethod scanner ((state state)
		    (words list)
		    (chart-listing chart-listing)
		    (lexicon lexicon))
  "Check if the next symbol of `state' is a member of the
 post-categories for the current word. As a side effect, enqueue
 a new state corresponding to th is find, into the current
 chart."
  (let* ((follow (follow-symbol state))
         (j (state-dot-index state))
         (word (nth j words)))
    (when (> *earley-debug* 2)
      (earley-msg
       (format "  scanner checking if \"%s\" is in %s"
	       follow
	       (format-token-list (lexicon-lookup word lexicon)))))
    (when (cl-member follow (lexicon-lookup word lexicon)
		  :test 'follow-match?)
      (let ((new-state (make-state :lhs follow
				   :rhs (list word)
				   :dot 1
				   :constituent-index j
				   :dot-index (+ j 1))))
        (when (> *earley-debug* 2)
	  (earley-msg
	   (format "  scanner enqueuing into chart %d if new\n\t%s"
		   (+ j 1) (format-state new-state))))
        (enqueue
	 new-state (nth (+ j 1) (chart-listing-charts chart-listing)))))))

(defun completer (state chart-listing)
  "Find and return a list of the previous states that expect this
state's symbol at this dot-index with the dot moved one step
forward. As a side-effect, enqueue the states in the current
chart."
  (let ((B (state-lhs state))
        (j (state-constituent-index state))
        (k (state-dot-index state)))
    (loop for prev-state
       in (chart-states (nth j (chart-listing-charts chart-listing)))
       when (let ((A (follow-symbol prev-state)))
	      (when (equal A B)
		(when (> *earley-debug* 3)
		  (earley-msg (format "  completer found the state:\n\t%s to match %s"
				      (format-state prev-state) state)))
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
		 (when (> *earley-debug* 2)
		   (earley-msg
		    (format "  reduction into chart %d if new:\n\t%s"
			    k (format-state new-state))))
		 (enqueue
		  new-state (nth k (chart-listing-charts chart-listing)))
		 new-state))))

(defun earley-parse (sentence grammar lexicon)
  "Parse the input string of words into a chart conforming to the grammar.
   Afterwards you can check to see if the parse covered the entire string."
  (let* ((words (remove "" (split-string sentence "[ \t\n]+")))
	 (start-symbol (grammar-start-symbol grammar))
	 (chart-listing (make-chart-listing :start-symbol start-symbol)))

    (assert (stringp start-symbol) nil
		     "Grammar should have a nonnil start-symbol set")

    (earley-msg-clear)
    ;; (setf (chart-listing-start-symbol chart-listing) start-symbol)
    ;; Initialize charts, one chart per word in the sentence
    (loop for i from 0 to (length words)
       do (add-chart (make-chart) chart-listing))

    ;; FIXME check that G is not in grammar but start-symbol is!

    ;; Start off by enqueuing a dummy state in the first chart
    (enqueue (make-state :lhs "G"
			 :rhs (list start-symbol)
			 :dot-index 0)
             (nth 0 (chart-listing-charts chart-listing)))

    ;; Then for each chart (= one per word)...
    (loop for chart in (chart-listing-charts chart-listing)
       and chart-index from 0
       ;; And for each possible state in that chart
       do (progn
	    (when (> *earley-debug* 0)
	      (earley-msg
	       (format "---- Processing Chart %s ----" chart-index)))
	    (loop for state-index from 0
	       until (>= state-index (length (chart-states chart)))
	       do (let ((state (nth state-index (chart-states chart))))
		    (when (> *earley-debug* 1)
		      (earley-msg
		       (format "Considering%s rule:\n\t%s"
			       (if (incomplete? state) " unfinished" "")
			       (format-state state))))
		    (cond ((and (incomplete? state)
				(not (cl-member (follow-symbol state)
					     (lexicon-part-of-speech lexicon)
					     :test 'equal)))
			   (when (> *earley-debug* 1)
			     (earley-msg "Predicting..."))
			   (predictor state chart-listing grammar))
			  ((and (incomplete? state)
				(cl-member (follow-symbol state)
					(lexicon-part-of-speech lexicon)
					:test 'equal))
			   (unless (eq chart (first
					      (last
					       (chart-listing-charts
						chart-listing))))
			     (when (> *earley-debug* 1)
			       (earley-msg "Scanning..."))
			     (scanner state words chart-listing lexicon)))
			  (t
			   (when (> *earley-debug* 1)
			     (earley-msg "Reductions..."))
			   (completer state chart-listing)))))
	    (when (> *earley-debug* 0)
	      (earley-msg ""))))
    (earley-msg "==============================================")
    chart-listing))

(provide-me "earley-parser:")
