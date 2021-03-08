

(defvar KeyWords (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(defvar KW_LIST (list "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(defvar Operators (list "+" "-" "/" "*" "**" "(" ")" "\"" "\"" ","))
(defvar OP_LIST (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_DBLMULT" "OP_OP" "OP_CP" "OP_OC" "OP_CC" "OP_COMMA"))

(defvar op_oc 0)

(defvar tokens (list))




(defun isOP (op)
	(let ((index 0))
	  	(loop for temp in Operators
	  		do
	  			(if (string= temp op)
	  				(if (string= op "\"")
	  					(let ()
	  						(setf index (+ op_oc index))
	  						(if (= 1 op_oc) (setf op_oc 0) (setf op_oc 1))
	  						(return-from isOP index)
	  					)
	  					(return-from isOP index)
	  				)
	  				(setf index (+ 1 index))
		  		)
		)
		(if (= index (length Operators))
			-1
			index
		)
  	)
)

(defun isValue(value)
	(let ((index 0) (ch) (err 0))
		(setf ch (char value 0))
		(if (not(digit-char-p ch)) (return-from isValue 0))
		(if (= (length value) 1) (return-from isValue 1))

		(loop 
			do

			(setf ch (char value index))
			(if (not(digit-char-p ch)) (setf err 1))
			(setq index (+ 1 index))

			(when (or (= index (length value)) (/= (isOP (string (char value index))) -1)) (return index) )
		)

		(if (= err 1) 0 index)
  	)
)

(defun isKeyWord(keyword)
	(let ((index 0) (ch) (word nil)(err 0) (kIndex 0))
		(setf ch (char keyword 0))
		(if (not(alpha-char-p ch)) (return-from isKeyWord (list -1 index)))
		(if (= (length keyword) 1) (return-from isKeyWord (list -1 index)))
		(setf keyword (string-downcase keyword))

		(loop 
			do

			(setf ch (char keyword index))
			(if (not(alpha-char-p  ch)) (setf err 1))
			(setf word ( concatenate 'string word (string ch)))
			(setq index (+ 1 index))

			(when (or (= index (length keyword)) (/= (isOP (string (char keyword index))) -1)) (return index) )
		)

		(if (= err 1) (return-from isKeyWord (list -1 index)))

		(loop for temp in KeyWords
	  		do
	  			(if (string= temp word)
	  				(return kIndex)
	  				(setf kIndex (+ 1 kIndex))
		  		)
		)
		(if (= kIndex (length KeyWords)) (list -1 index) (list kIndex index))
			

  	)

)

(defun isID(ID)
	(let ((index 0) (ch) (err 0))
		(setf ch (char ID 0))
		(if (not(alpha-char-p ch)) (return-from isID 0))
		(if (= (length ID) 1) (return-from isID 1))

		(loop 
			do

			(setf ch (char ID index))
			(if (not (or (alpha-char-p  ch) (digit-char-p ch) (char= #\_ ch) )) (setf err 1))
			(setq index (+ 1 index))

			(when (or (= index (length ID)) (/= (isOP (string (char ID index))) -1)) (return index) )
		)
		
		(if (= err 1) 0 index)
  	)
)


(defun returnUnknown(unknown)
	(let ((index 0) (ch) (word nil))
		(setf ch (char unknown 0))
		(if (= (length unknown) 1) (return-from returnUnknown unknown))

		(loop 
			do

			(setf ch (char unknown index))
			(setf word ( concatenate 'string word (string ch)))
			(setq index (+ 1 index))
			
			(when (or (= index (length unknown)) (/= (isOP (string (char unknown index))) -1)) (return index) )
		)
		word
  	)
)

(defun tokenize-word(word ostream)
	(let ((index 0) (ch) (rIndex) (len) (subword nil) lIndex)
		
		(setf len (length word))
		(loop while (/= index len)
			do
			(setf ch (string (char word index)))
			(setf subword (subseq word index))

			(cond
				((/= (setq rIndex (isOP ch)) -1)
					(let ()
						(setf index (+ 1 index))

						(if (string= (nth rIndex Operators) "*")
							(prog ((tempch) (trIndex))
								(if (= len index) (return 0))
								(setf tempch (string (char word index)))

								(if (and (/= (setq trIndex (isOP tempch)) -1) (string= (nth trIndex Operators) "*"))
									(let () 
										(setf rIndex (+ 1 rIndex))
										(setf index (+ 1 index))
									)
								)
							)	
						)
						(print (nth rIndex OP_LIST))
						(format ostream "~A~c" (nth rIndex OP_LIST) #\NewLine)
					)
				)
				((/= (setq rIndex (isValue subword)) 0)
					(let ()

						(setf index (+ index rIndex))
						(format ostream "VALUE~c" #\NewLine)
						(print "VALUE")
					)
				)	
				((/= -1 (car (setq lIndex (isKeyWord subword))))
					(let ()
						(print (nth (car lIndex) KW_LIST))
						(format ostream "~A~c" (nth (car lIndex) KW_LIST) #\NewLine)
						(setf index (+ index (cadr lIndex)))
					)

				)
				((/= (setq rIndex (isID subword)) 0)
					(let ()

						(setf index (+ index rIndex))
						(print "IDENTIFIER")
						(format ostream "IDENTIFIER~c" #\NewLine)
					)
				)
				( t
					(let (tempWord)
						(setf tempWord (returnUnknown subword))
						(format t "~cSYNTAX_ERROR ~A cannot be tokenized" #\NewLine tempWord)
						(format ostream "SYNTAX_ERROR ~A cannot be tokenized~c" tempWord #\NewLine)
						(setf index (+ index (length tempWord)))
					)	
				)
			)	
		)
  	)
)

(defun tokenize-line(line ostream)
	(let ((wordList nil) (comment 0))
		(setf wordList (split-line line))
		(loop for temp in wordList
	  		do
	  			(if (and (> (length temp) 1) (char= (char temp 0) #\;) (char= (char temp 1) #\;))
	  				(let () (print "COMMENT") (format ostream "COMMENT~c" #\NewLine) (return temp))
	  				(tokenize-word temp ostream)
	  			)
		)

	)
)

(defun split-line (line)
  (let ((word nil) (index 0) (ch) (wordList (list)))
  	(loop 

  		(setf ch (string (char line index)))
		(cond
			((or (string= ch " ") (string= ch #\Tab)) (if (not (eql word nil)) (let () (setf wordList (cons word wordList)) (setf word nil))))
			( t (setf word ( concatenate 'string word ch )))
		)

		(setf index (+ index 1))

		(when (= (length line) index) (return wordList))
	)
	(if (not (eql word nil)) (setf wordList (cons word wordList)) )
	(reverse wordList)
  )
)

(defun gppinterpreter()

	(let ((line nil))
		(cond 
			((= (length *args*) 0)
				(let ()
					(with-open-file (ostream "parsed_lisp.txt" :direction :output)
						(format t "~c> " #\NewLine)
						(setf line (read-line))	

						(loop while(string/= line "")
							do
							;;(setf op_oc 0)
							(tokenize-line line ostream)
							(format t "~c~c> " #\NewLine #\NewLine)
							(setf line (read-line))	
						)
					)	
				)
			)
			((= (length *args*) 1)
				(prog ()
					(with-open-file (ostream "parsed_lisp.txt" :direction :output)
						(with-open-file (istream (car *args*) :if-does-not-exist nil :direction :input)

							(if (eql istream nil) (return (format t "~cFile does not exist.~c" #\NewLine #\NewLine)))

						    (setf line (read-line istream nil))	

							(loop while(not(eql line nil))
								do
								(if (string/= line "") (tokenize-line line ostream))
								(setf line (read-line istream nil))	
							)
						)	
					)
				)
			)
			( 	t
				(format t "~cToo many arguments given as parameters.~c" #\NewLine #\NewLine)
			)
		)
	)
)


(gppinterpreter)





