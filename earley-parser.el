;;;  -*- lexical-binding: t -*-

(require 'eieio)
(require 'cl)
(require 'load-relative)
(load-relative "./objects")
(load-relative "./terminal")

(defmethod predictor ((state state)
		      (chart-listing chart-listing)
		      (grammar grammar))
  "Predict possible successor states based on the grammar. As a side-effect, add
 these states to the chart that this state belong to."
  (let ((B (next-cat state))
        (j (state-dot-index state)))
    (loop for production in (grammar-productions B grammar)
       collect (let ((new-state
		      (make-state :condition B
				  :subtree production
				  :dot 0
				  :constituent-index j
				  :dot-index j)))
		 (when (> *earley-debug* 2)
		   (message "  predictor attempting to enqueue")
		   (message " %s into chart %s" new-state j))
		 (enqueue new-state (nth j (chart-listing-charts
					    chart-listing)))
		 new-state))))

(defmethod scanner ((state state)
		    (words list)
		    (chart-listing chart-listing)
		    (lexicon lexicon))
  "Check if the next category for this state is a member of the pos-cathegories
 for the current word. As a side effect, enqueue a new state corresponding to th
is find, into the current chart."
  (let* ((B (next-cat state))
         (j (state-dot-index state))
         (word (nth j words)))
    (when (> *earley-debug* 2)
      (message "  scanner is considering if %s is a member of" B)
      (message " the word-class list for \"%s\" (= %s)"
	      word (lexicon-lookup word lexicon)))
    (when (cl-member B (lexicon-lookup word lexicon)
		  :test (function (lambda (b terminal)
			    (funcall *string-comparer*
				     b (terminal-class terminal)))))
      (let ((new-state (make-state :condition B
				   :subtree (list word)
				   :dot 1
				   :constituent-index j
				   :dot-index (+ j 1))))
        (when (> *earley-debug* 2)
          (message "scanner attempting to enqueue")
	  (message "%s into chart %s"  new-state (+ j 1)))
        (enqueue
	 new-state (nth (+ j 1) (chart-listing-charts chart-listing)))))))

(defun completer ((state state) (chart-listing chart-listing))
  "Find and return a list of the previous states that expect this
states category at this dot-index with the dot moved one step
forward. As a side-effect, enqueue the states in the current
chart."
  (let ((B (state-condition state))
        (j (state-constituent-index state))
        (k (state-dot-index state)))
    (loop for prev-state
       in (chart-states (nth j (chart-listing-charts chart-listing)))
       when (let ((A (next-cat prev-state)))
	      (when (funcall *string-comparer* A B)
		(when (> *earley-debug* 3)
		  (format
		   t "    completer found the state: %s to match %s"
		   prev-state state))
		t))
       collect (let ((new-state (make-state
				 :condition (state-condition prev-state)
				 :subtree (state-subtree prev-state)
				 :dot (+ (state-dot prev-state) 1)
				 :constituent-index (state-constituent-index
						     prev-state)
				 :dot-index k
				 :source-states (append (list state)
							(state-source-states
							 prev-state)))))
		 (when (> *earley-debug* 2)
		   (message "  completer attempting to enqueue")
		   (message " %s into chart %s" new-state k))
		 (enqueue
		  new-state (nth k (chart-listing-charts chart-listing)))
		 new-state))))

(defun earley-parse (sentence grammar lexicon &optional start-symbol)
  "Convert a string of words into a chart conforming to the grammar."
  (let ((words (remove "" (split-string sentence "[ \t\n]+")))
	(start-sym (or start-symbol "S"))
        (chart-listing (make-chart-listing)))

    ;; Initialize charts, one chart per word in the sentence
    (loop for i from 0 to (length words)
       do (add-chart (make-chart) chart-listing))

    ;; FIXME check that G is not in grammar but start-sym is!

    ;; Start off by enqueuing a dummy state in the first chart
    (enqueue (make-state :condition "G"
			 :subtree (list start-sym)
			 :dot-index 0)
             (nth 0 (chart-listing-charts chart-listing)))

    ;; Then for each chart (= one per word)...
    (loop for chart in (chart-listing-charts chart-listing)
       and chart-index from 0
       ;; And for each possible state in that chart
       do (progn
	    (when (> *earley-debug* 0)
	      (message "---- processing chart %s ----" chart-index))
	    (loop for state-index from 0
	       until (>= state-index (length (chart-states chart)))
	       do (let ((state (nth state-index (chart-states chart))))
		    (when (> *earley-debug* 1)
		      (message "considering state: %s" state)
		      (message "next cat of this %s state is %s"
			      (if (incomplete? state) " (incomplete) " " ")
			      (next-cat state)))
		    (cond ((and (incomplete? state)
				(not (cl-member (next-cat state)
					     (lexicon-part-of-speech lexicon)
					     :test equal)))
			   (when (> *earley-debug* 1)
			     (message "predicting..."))
			   (predictor state chart-listing grammar))
			  ((and (incomplete? state)
				(cl-member (next-cat state)
					(lexicon-part-of-speech lexicon)
					:test equal))
			   (unless (eq chart (first
					      (last
					       (chart-listing-charts
						chart-listing))))
			     (when (> *earley-debug* 1)
			       (message "scanning..."))
			     (scanner state words chart-listing lexicon)))
			  (t
			   (when (> *earley-debug* 1)
			     (message "completing..."))
			   (completer state chart-listing)))))
	    (when (> *earley-debug* 0)
	      (message ""))))
    chart-listing))

(provide 'earley-parser)
