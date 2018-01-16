;;;  -*- lexical-binding: t -*-

(require 'eieio)
(require 'cl)
(require 'load-relative)
(load-relative "./objects")
(load-relative "./tokens")
(load-relative "./msg")

(defun predictor (state chart-listing grammar)
  "Predict possible successor states based on the grammar. As a side-effect, add
 these states to the chart that this state belong to."
  (let ((B (follow-symbol state))
        (j (state-dot-index state)))
    (loop for production in (grammar-productions B grammar)
       collect (let ((new-state
		      (make-state :condition B
				  :subtree production
				  :dot 0
				  :constituent-index j
				  :dot-index j)))
		 (when (> *earley-debug* 2)
		   (earley-msg (format "  predictor enqueuing\n\t%s\n  into chart %s"
				       (format-state new-state) j)))

		 (enqueue new-state (nth j (chart-listing-charts
					    chart-listing)))
		 new-state))))

(defun follow-match?(follow-symbol terminal)
    (equal follow-symbol (terminal-class terminal)))

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
         (word (nth j words))
	 (mess))
    (when (> *earley-debug* 2)
      (earley-msg
       (format "  scanner checking if \"%s\" is in %s"
	       follow
	       (format-terminal-list (lexicon-lookup word lexicon)))))
    (when (cl-member follow (lexicon-lookup word lexicon)
		  :test 'follow-match?)
      (let ((new-state (make-state :condition follow
				   :subtree (list word)
				   :dot 1
				   :constituent-index j
				   :dot-index (+ j 1))))
        (when (> *earley-debug* 2)
	  (earley-msg
	   (format "  scanner enqueuing\n\t%s\n  into chart %d if new"
		   (format-state new-state) (+ j 1))))
        (enqueue
	 new-state (nth (+ j 1) (chart-listing-charts chart-listing)))))))

(defun completer (state chart-listing)
  "Find and return a list of the previous states that expect this
state's symbol at this dot-index with the dot moved one step
forward. As a side-effect, enqueue the states in the current
chart."
  (let ((B (state-condition state))
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
		   (earley-msg
		    (format "  completer enqueuing:\n\t%s\n  into chart %d if new"
			    (format-state new-state) k)))
		 (enqueue
		  new-state (nth k (chart-listing-charts chart-listing)))
		 new-state))))

(defun earley-parse (sentence grammar lexicon &optional start-symbol)
  "Convert a string of words into a chart conforming to the grammar."
  (let ((words (remove "" (split-string sentence "[ \t\n]+")))
        (chart-listing (make-chart-listing)))

    (earley-msg-clear)
    (unless start-symbol (setq start-symbol "S"))
    ;; Initialize charts, one chart per word in the sentence
    (loop for i from 0 to (length words)
       do (add-chart (make-chart) chart-listing))

    ;; FIXME check that G is not in grammar but start-symbol is!

    ;; Start off by enqueuing a dummy state in the first chart
    (enqueue (make-state :condition "G"
			 :subtree (list start-symbol)
			 :dot-index 0)
             (nth 0 (chart-listing-charts chart-listing)))

    ;; Then for each chart (= one per word)...
    (loop for chart in (chart-listing-charts chart-listing)
       and chart-index from 0
       ;; And for each possible state in that chart
       do (progn
	    (when (> *earley-debug* 0)
	      (earley-msg
	       (format "---- processing chart %s ----" chart-index)))
	    (loop for state-index from 0
	       until (>= state-index (length (chart-states chart)))
	       do (let ((state (nth state-index (chart-states chart))))
		    (when (> *earley-debug* 1)
		      (earley-msg
		       (format "Considering%s state:\n\t%s"
			       (if (incomplete? state) " incomplete" "")
			       (format-state state))))
		      ;; (earley-msg
		      ;;  (format "  follow symbol of this%s state is %s"
		      ;; 	       (if (incomplete? state) " (incomplete)" "")
		      ;; 	       (follow-symbol state))))
		    (cond ((and (incomplete? state)
				(not (cl-member (follow-symbol state)
					     (lexicon-part-of-speech lexicon)
					     :test 'equal)))
			   (when (> *earley-debug* 1)
			     (earley-msg "predicting..."))
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
			       (earley-msg "scanning..."))
			     (scanner state words chart-listing lexicon)))
			  (t
			   (when (> *earley-debug* 1)
			     (earley-msg "completing..."))
			   (completer state chart-listing)))))
	    (when (> *earley-debug* 0)
	      (earley-msg ""))))
    (earley-msg "==============================================")
    chart-listing))

(provide 'earley-parser)
