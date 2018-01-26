;;;  -*- lexical-binding: t -*-
(require 'eieio)
(require 'cl-lib)
(require 'load-relative)
(load-relative "./tokens")

;; The context free grammar representation is a hashtable keyed on non-terminal
;; symbols. For each symbol there is a list of productions/rules for that
;; symbol. The productions take the form of a ordered list of symbols.
;;
;; The non-terminal symbols are represented as strings.


;; The lexicon is the representation of all terminal symbols (words in the
;; language). It is a hashtable keyed on word. For each word there is a list
;; of "terminal" structures that are different semantic mappings of the word.



;; ;;;; Backus-Naur Form grammar reader functions
;; ;;;;------------------------------------------
;; (defun load-bnf-grammar (pathname)
;;   "Reads a grammar on Backus-Naur form into the representation of the context
;;    free grammar(CFG)."
;;   (let ((rules (make-hash-table :test 'equal)))
;;     ;; For each production in the BNF file, create a list of possible ordered
;;     ;; symbol sequences (list of strings) that is a legal expression for the
;;     ;; symbol
;;     (with-open-file (file pathname :direction :input)
;;       (loop for production = (read-next-bnf-production file)
;; 	    until (null production)
;; 	    do (labels ((inject-expansions! (symbol-list expansion)
;; 			  (cond
;; 			   ;; No more expansions -> Inject last one
;; 			   ((null symbol-list)
;; 			    (push expansion (gethash (car production) rules)))
;; 			   ;; More expansion -> Inject current and continue
;; 			   ((equalp (first symbol-list) "|")
;; 			    (push expansion (gethash (car production) rules))
;; 			    (inject-expansions! (cdr symbol-list) nil))
;; 		       ;; Expansion not ended -> Collect rest of expansion
;; 			   (t
;; 			    (inject-expansions!
;; 			     (cdr symbol-list)
;; 			     (append expansion (list (first symbol-list))))))))
;; 		       (inject-expansions! (cddr production) nil))))
;;     (make-grammar :rules rules)))

;; (let ((token-cache))
;;   (defun read-next-bnf-production (file &optional (keep-newline nil))
;;     "Reads and returns the next Backus-Naur production from file."
;;     (let ((production))
;;       ;; If there is anything in the cache, pop it off and add it to production
;;       (loop until (null token-cache)
;; 	 do (push (pop token-cache) production))
;;       (loop for token = (read-next-grammar-token file keep-newline)
;; 	 until (null token)
;; 	 do (if
;; 	     ;; If we just read a "::=" and there already is one in the
;; 	     ;; production -> Push it and last token onto cahce instead of
;; 	     ;; production
;; 	     (and
;; 	      (equalp token "::=") (cl-member token production :test *string-comparer*))
;; 	     (progn
;; 	       (push token token-cache)
;; 	       (push (pop production) token-cache)
;; 	       (return))
;; 	     ;; Otherwise -> Keep adding tokens to the production
;; 	     (push token production)))
;;       (reverse production))))

(defun earley:read-next-grammar-token (&optional opt-buffer keep-newline)
  "Reads and returns the next grammr token from the current buffer.
   buffer should be a BNF grammar."
  (let ((token "")
	(buffer (or opt-buffer (current-buffer))))
    (with-current-buffer buffer
      (loop for char = (char-after)
	    until (eobp)
	    do
	    (forward-char 1)
	    (cond
	     ;; If the char is an '=' and the rest of the token is already
	     ;; "::" -> Return token
	     ((and (char-equal char ?=) (string= token "::"))
	      (setf token (concat token (string char)))
	      (return))
	     ;; If the char is an '>' and token starts with an '<' ->
	     ;; Return token
	     ((and (char-equal char ?>) (substring token 0 1) "<")
	      (setf token (concat token (string char)))
	      (return))
	     ;; If the char is '|' it is in it self a complete token
	     ((and (char-equal char ?|) (= (length token) 0))
	      (setf token (concat token (string char)))
	      (return))
	     ;; Newlines are also (or may be) complete tokens
	     ((and (char-equal char ?\n) (= (length token) 0)
		   keep-newline)
	      (setf token (concat token (string char)))
	      (return))
	     ;; If the char is whitespace, and token is empty ->
	     ;; Do nothing, just continue with the next char
	     ((and (string-match "^[ \t\n]" (string char))
		   (= (length token) 0)))
	     ;; If nothing specific matches -> Add char to token
	     (t
	      (setf token (concat token (string char))))))
      (when (> (length token) 0)
	(replace-regexp-in-string "^<\\(.+\\)>" "\\1" token)))))



;;;; Lexicon functions
;;;;------------------
;;;; Reads a dictionary on the form:
;;;;
;;;; <token-name> :class <class>
;;;;
;;;; into a hashtable of lists of tokens and
;;;; a list of word classes (part of speech).

(cl-defmethod earley:parse-lexicon-line ((line string))
  "Parse the string of the form:
<token-name> :class <class>
and return a token created from that"

  (let* ((tokens (remove "" (split-string line "[ \t]+")))
	 (token-val (first tokens))
	 (l (rest tokens))
	 (options nil)
	 (str))
    (loop
     while l
     do
     (setq str (car l))
     (setq l (cdr l))
     (when (string-prefix-p ":" str)
       (setq options (append options (list (cons str (car l)))))
       (setq l (cdr l))))
    ;; Create the word object
    (make-token :value token-val
		:class (cdr (assoc ":class" options)))))

(cl-defmethod earley:load-lexicon-from-string ((str string))
  "Parse a string into a lexicon object and return that."
  (let ((lexicon (make-hash-table :test 'equal))
	(token-alphabet nil))
    (loop for line in (remove "" (split-string str "[\n]+"))
	  do (let ((token (earley:parse-lexicon-line line)))
	    ;; Add (unless existing) the token class to the list of token-classes
	    (pushnew (token-class token) token-alphabet :test 'equal)
	    ;; Add the token value and its association to the lexicon
	    (push token (gethash (token-value token) lexicon))))
    (make-lexicon :token-dict lexicon :token-alphabet token-alphabet)))

(provide-me "earley-parser:")
