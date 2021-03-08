

(defvar KeyWords (list "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(defvar KW_LIST (list "KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(defvar Operators (list "+" "-" "/" "*" "**" "(" ")" "\"" "\"" ","))
(defvar OP_LIST (list "OP_PLUS" "OP_MINUS" "OP_DIV" "OP_MULT" "OP_DBLMULT" "OP_OP" "OP_CP" "OP_OC" "OP_CC" "OP_COMMA"))

(defvar op_oc 0)

(defvar tokens (list))
(defvar tokenString (list))

(defvar varList (list))
(defvar varValues (list))




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

(defun tokenize-word(word)
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
						(push (nth rIndex OP_LIST) tokens)
						(push (nth rIndex Operators) tokenString)
						(nth rIndex OP_LIST)
					)
				)
				((/= (setq rIndex (isValue subword)) 0)
					(let ()

						(setf index (+ index rIndex))
						(push "VALUE" tokens)
						(push (subseq subword 0 rIndex) tokenString)
					)
				)	
				((/= -1 (car (setq lIndex (isKeyWord subword))))
					(let ()
						(push (nth (car lIndex) KW_LIST) tokens)
						(push (nth (car lIndex) KeyWords) tokenString)
						(nth (car lIndex) KW_LIST)
						(setf index (+ index (cadr lIndex)))
					)

				)
				((/= (setq rIndex (isID subword)) 0)
					(let ()

						(setf index (+ index rIndex))
						(push "IDENTIFIER" tokens)
						(push (subseq subword 0 rIndex) tokenString)
					)
				)
				( t
					(let (tempWord)
						(setf tempWord (returnUnknown subword))
						(setf index (+ index (length tempWord)))
					)	
				)
			)	
		)
  	)
)

(defun tokenize-line(line)
	(let ((wordList nil) (comment 0))
		(setf wordList (split-line line))
		(loop for temp in wordList
	  		do
	  			(if (and (> (length temp) 1) (char= (char temp 0) #\;) (char= (char temp 1) #\;))
	  				(let () (push "COMMENT" tokens) (push temp tokenString) (return temp))
	  				(tokenize-word temp)
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
					(with-open-file (ostream "output.txt" :direction :output)
						(format t "~c> " #\NewLine)
						(setf line (read-line))	

						(loop while(string/= line "")
							do
							(setq tokens (list))
							(setq tokenString (list))

							(tokenize-line line)

							(setq tokens (reverse tokens))

							(setq tokenString (reverse tokenString))
							
							(if (not (equal (car tokens) "COMMENT" ))

								(let ()

									(setq result (INPUT))

									(if (equal (cadr result) -1) 
										(let ()
											(format t "SYNTAX_ERROR Expression not recognized ~c" #\NewLine)
											(format ostream "SYNTAX_ERROR Expression not recognized ~c" #\NewLine)
										)
										(let()
											(setq result (car result))
											(format t "Syntax OK.~c" #\NewLine)
											(format t "Result: ~d ~c" result #\NewLine)
											(format ostream "Syntax OK.~c" #\NewLine)
											(format ostream "Result: ~d~c" result #\NewLine)
										)
									)
								)

								(let ()
									(format t "Syntax OK.~c" #\NewLine)
									(format t "Result: COMMENT ~c" #\NewLine)
									(format ostream "Syntax OK.~c" #\NewLine)
									(format ostream "Result: COMMENT ~c" #\NewLine)
								)

							)

							
							(format ostream "~c" #\NewLine)
							(format t "~c~c> " #\NewLine #\NewLine)
							(setf line (read-line))	
						)
					)	
				)
			)
			((= (length *args*) 1)
				(prog ()
					(with-open-file (ostream "output.txt" :direction :output)
						(with-open-file (istream (car *args*) :if-does-not-exist nil :direction :input)

							(if (eql istream nil) (return (format t "~cFile does not exist.~c" #\NewLine #\NewLine)))

						    (setf line (read-line istream nil))	

							(loop while(not(eql line nil))
								do
								(setq tokens (list))
								(setq tokenString (list))

								(if (string/= line "") (tokenize-line line))

								(setq tokens (reverse tokens))

								(setq tokenString (reverse tokenString))
								
								(if (not (equal (car tokens) "COMMENT" ))

									(let ()

										(setq result (INPUT))

										(if (equal (cadr result) -1) 
											(let ()
												(format t "SYNTAX_ERROR Expression not recognized ~c" #\NewLine)
												(format ostream "SYNTAX_ERROR Expression not recognized ~c" #\NewLine)
											)
											(let()
												(setq result (car result))
												(format t "Syntax OK.~c" #\NewLine)
												(format t "Result: ~d ~c" result #\NewLine)

												(format ostream "Syntax OK.~c" #\NewLine)
												(format ostream "Result: ~d~c" result #\NewLine)
											)
										)
									)

									(let ()
										(format t "Syntax OK.~c" #\NewLine)
										(format t "Result: COMMENT ~c" #\NewLine)
										(format ostream "Syntax OK.~c" #\NewLine)
										(format ostream "Result: COMMENT ~c" #\NewLine)
									)

								)
								(format ostream "~c" #\NewLine)
								(format t "~c" #\NewLine)
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


(defun EXPI(ExpTo ExpS)
	(prog ( (len (list-length ExpTo)) )
		(cond
			( (and (= len 1) (string= (car ExpTo) "VALUE")) (return (parse-integer (car ExpS))) )

			( (and (= len 1) (string= (car ExpTo) "IDENTIFIER")) (return (nth (varValue (car ExpS)) varValues)) )

			;; + operator
			( (and (> len 4) (string= (first ExpTo) "OP_OP") (string= (car (last ExpTo)) "OP_CP") (string= (second ExpTo) "OP_PLUS") ) 
				(prog ((LO) (RO) (lIndex) (rIndex) (lExp) (rExp) (nExpTo)  (nExpS) (lExpS) (rExpS))

					(setq nExpTo (subseq ExpTo 2 (- len 1)))
					(setq nExpS (subseq ExpS 2 (- len 1)))
					(setq lIndex (divideEXPI nExpTo))

					(if (/= lIndex 0) (setq rIndex (divideEXPI (subseq nExpTo lIndex))))

					(if (and (/= lIndex 0) (/= rIndex 0) (= (+ 3 lIndex rIndex) len))
						(prog ()
							(setq lExp (subseq nExpTo 0 lIndex))
							(setq rExp (subseq nExpTo lIndex))

							(setq lExpS (subseq nExpS  0 lIndex))
							(setq rExpS (subseq nExpS lIndex))

							(setq LO (EXPI lExp lExpS))
							(setq RO (EXPI rExp rExpS))

							(if (and (not (eql LO nil)) (not (eql RO nil)) )
								(return-from EXPI (+ LO RO) )
								(return-from EXPI nil)
							)

						)

						(return-from EXPI nil)
					)
				)
			)

			;; - operator
			( (and (> len 4) (string= (first ExpTo) "OP_OP") (string= (car (last ExpTo)) "OP_CP") (string= (second ExpTo) "OP_MINUS") ) 
				(prog ((LO) (RO) (lIndex) (rIndex) (lExp) (rExp) (nExpTo)  (nExpS) (lExpS) (rExpS))

					(setq nExpTo (subseq ExpTo 2 (- len 1)))
					(setq nExpS (subseq ExpS 2 (- len 1)))
					(setq lIndex (divideEXPI nExpTo))

					(if (/= lIndex 0) (setq rIndex (divideEXPI  (subseq nExpTo lIndex))))

					(if (and (/= lIndex 0) (/= rIndex 0) (= (+ 3 lIndex rIndex) len))
						(prog ()
							(setq lExp (subseq nExpTo 0 lIndex))
							(setq rExp (subseq nExpTo lIndex))

							(setq lExpS (subseq nExpS  0 lIndex))
							(setq rExpS (subseq nExpS lIndex))

							(setq LO (EXPI lExp lExpS))
							(setq RO (EXPI rExp rExpS))

							(if (and (not (eql LO nil)) (not (eql RO nil))  )
								(return-from EXPI (- LO RO) )
								(return-from EXPI nil)
							)

						)

						(return-from EXPI nil)
					)
				)
			)

			;; * operator
			( (and (> len 4) (string= (first ExpTo) "OP_OP") (string= (car (last ExpTo)) "OP_CP") (string= (second ExpTo) "OP_MULT") ) 
				(prog ((LO) (RO) (lIndex) (rIndex) (lExp) (rExp) (nExpTo)  (nExpS) (lExpS) (rExpS))

					(setq nExpTo (subseq ExpTo 2 (- len 1)))
					(setq nExpS (subseq ExpS 2 (- len 1)))
					(setq lIndex (divideEXPI nExpTo))

					(if (/= lIndex 0) (setq rIndex (divideEXPI (subseq nExpTo lIndex))))

					(if (and (/= lIndex 0) (/= rIndex 0) (= (+ 3 lIndex rIndex) len))
						(prog ()
							(setq lExp (subseq nExpTo 0 lIndex))
							(setq rExp (subseq nExpTo lIndex))

							(setq lExpS (subseq nExpS  0 lIndex))
							(setq rExpS (subseq nExpS lIndex))

							(setq LO (EXPI lExp lExpS))
							(setq RO (EXPI rExp rExpS))

							(if (and (not (eql LO nil)) (not (eql RO nil))  )
								(return-from EXPI (* LO RO) )
								(return-from EXPI nil)
							)

						)

						(return-from EXPI nil)
					)
				)
			)

			;; / operator
			( (and (> len 4) (string= (first ExpTo) "OP_OP") (string= (car (last ExpTo)) "OP_CP") (string= (second ExpTo) "OP_DIV") ) 
				(prog ((LO) (RO) (lIndex) (rIndex) (lExp) (rExp) (nExpTo)  (nExpS) (lExpS) (rExpS))

					(setq nExpTo (subseq ExpTo 2 (- len 1)))
					(setq nExpS (subseq ExpS 2 (- len 1)))
					(setq lIndex (divideEXPI nExpTo))

					(if (/= lIndex 0) (setq rIndex (divideEXPI (subseq nExpTo lIndex))))

					(if (and (/= lIndex 0) (/= rIndex 0) (= (+ 3 lIndex rIndex) len))
						(prog ()
							(setq lExp (subseq nExpTo 0 lIndex))
							(setq rExp (subseq nExpTo lIndex))

							(setq lExpS (subseq nExpS  0 lIndex))
							(setq rExpS (subseq nExpS lIndex))

							(setq LO (EXPI lExp lExpS))
							(setq RO (EXPI rExp rExpS))

							(if (and (not (eql LO nil)) (not (eql RO nil))  )
								(return-from EXPI (/ LO RO) )
								(return-from EXPI nil)
							)

						)

						(return-from EXPI nil)
					)
				)
			)

			;; set
			( (and (> len 4) (string= (first ExpTo) "OP_OP") (string= (car (last ExpTo)) "OP_CP") (string= (second ExpTo) "KW_SET") ) 
				(prog ((LO) (index) (lExp) (lExpS) )

					(if (string= (third ExpTo) "IDENTIFIER")
						(prog ()
							(setq index (varValue (third ExpS)))


							(setq lExp (subseq ExpTo 3 (- len 1)))
							(setq lExpS (subseq ExpS 3 (- len 1)))
							(setq LO (EXPI lExp lExpS))

							(if (not (eql LO nil))   
								(return-from EXPI (setf (nth index varValues) LO) )
								(return-from EXPI nil)
							)

						)

						(return-from EXPI nil)
					)
				)
			)


			;; if EXB EXPI
			( (and (> len 4) (string= (first ExpTo) "OP_OP") (string= (car (last ExpTo)) "OP_CP") (string= (second ExpTo) "KW_IF") ) 
				(prog ((LO) (RO) (lIndex) (rIndex) (lExp) (rExp) (nExpTo)  (nExpS) (lExpS) (rExpS))

					(setq nExpTo (subseq ExpTo 2 (- len 1)))
					(setq nExpS (subseq ExpS 2 (- len 1)))
					(setq lIndex (divideEXPB nExpTo))

					(if (/= lIndex 0) (setq rIndex (divideEXPI (subseq nExpTo lIndex))))

					(if (and (/= lIndex 0) (/= rIndex 0) (= (+ 3 lIndex rIndex) len))
						(prog ()
							(setq lExp (subseq nExpTo 0 lIndex))
							(setq rExp (subseq nExpTo lIndex))

							(setq lExpS (subseq nExpS  0 lIndex))
							(setq rExpS (subseq nExpS lIndex))

							(setq LO (EXPB lExp lExpS))
							(setq RO (EXPI rExp rExpS))

							(if (and (not (eql LO nil)) (not (eql RO nil))  )
								(if (= LO 1) 
									(return-from EXPI RO)
									(return-from EXPI 0)
								)
								(return-from EXPI nil)
							)

						)

						(return-from EXPI nil)
					)
				)
			)

			(t (return-from EXPI nil))
		)
	)	
)


(defun EXPB(ExpTo ExpS)
	(prog ( (len (list-length ExpTo)) (BV) )
		(cond
			;; Binary Value
			( (and (= len 1) (string= (car ExpTo) "VALUE")) 

				(if (or (eql 0 (setq BV (parse-integer (car ExpS)))) (eql 1 BV) )
					(return BV)
					(return nil)
				)
			)
			;; true
			( (and (= len 1) (string= (car ExpTo) "KW_TRUE")) 

				(return 1)
			)

			;; false
			( (and (= len 1) (string= (car ExpTo) "KW_FALSE")) 

				(return 0)
			)

			;; Keyword and
			( (and (string= (first ExpTo) "OP_OP") (string= (car (last ExpTo)) "OP_CP") (string= (second ExpTo) "KW_AND") ) 
				(prog ((LO) (RO) (lIndex) (rIndex) (lExp) (rExp) (nExpTo)  (nExpS) (lExpS) (rExpS))
					(setq nExpTo (subseq ExpTo 2 (- len 1)))
					(setq nExpS (subseq ExpS 2 (- len 1)))
					(setq lIndex (divideEXPB nExpTo))

					(if (/= lIndex 0) (setq rIndex (divideEXPB (subseq nExpTo lIndex))))

					(if (and (/= lIndex 0) (/= rIndex 0) (= (+ 3 lIndex rIndex) len))
						(prog ()
							(setq lExp (subseq nExpTo 0 lIndex))
							(setq rExp (subseq nExpTo lIndex))

							(setq lExpS (subseq nExpS  0 lIndex))
							(setq rExpS (subseq nExpS lIndex))

							(setq LO (EXPB lExp lExpS))
							(setq RO (EXPB rExp rExpS))

							(if (and (not (eql LO nil)) (not (eql RO nil))  )
								(return-from EXPB (and_ LO RO) )
								(return-from EXPB nil)
							)

						)

						(return-from EXPB nil)
					)
				)
			)

			;; Keyword or
			( (and (string= (first ExpTo) "OP_OP") (string= (car (last ExpTo)) "OP_CP") (string= (second ExpTo) "KW_OR") ) 
				(prog ((LO) (RO) (lIndex) (rIndex) (lExp) (rExp) (nExpTo)  (nExpS) (lExpS) (rExpS))

					(setq nExpTo (subseq ExpTo 2 (- len 1)))
					(setq nExpS (subseq ExpS 2 (- len 1)))
					(setq lIndex (divideEXPB nExpTo))

					(if (/= lIndex 0) (setq rIndex (divideEXPB (subseq nExpTo lIndex)))) 

					(if (and (/= lIndex 0) (/= rIndex 0) (= (+ 3 lIndex rIndex) len))
						(prog ()
							(setq lExp (subseq nExpTo 0 lIndex))
							(setq rExp (subseq nExpTo lIndex))

							(setq lExpS (subseq nExpS  0 lIndex))
							(setq rExpS (subseq nExpS lIndex))

							(setq LO (EXPB lExp lExpS))
							(setq RO (EXPB rExp rExpS))

							(if (and (not (eql LO nil)) (not (eql RO nil))  )
								(return-from EXPB (or_ LO RO))
								(return-from EXPB nil)
							)

						)

						(return-from EXPB nil)
					)
				)
			)

			;; Keyword equal 
			( (and (string= (first ExpTo) "OP_OP") (string= (car (last ExpTo)) "OP_CP") (string= (second ExpTo) "KW_EQUAL") ) 
				(prog ((LO) (RO) (lIndex) (rIndex) (lExp) (rExp) (nExpTo)  (nExpS) (lExpS) (rExpS))

					(setq nExpTo (subseq ExpTo 2 (- len 1)))
					(setq nExpS (subseq ExpS 2 (- len 1)))
					(setq lIndex (divideEXPB nExpTo))

					(if (/= lIndex 0) (setq rIndex (divideEXPB (subseq nExpTo lIndex))))

					(if (and (/= lIndex 0) (/= rIndex 0) (= (+ 3 lIndex rIndex) len))
						(prog ()
							(setq lExp (subseq nExpTo 0 lIndex))
							(setq rExp (subseq nExpTo lIndex))

							(setq lExpS (subseq nExpS  0 lIndex))
							(setq rExpS (subseq nExpS lIndex))

							(setq LO (EXPB lExp lExpS))
							(setq RO (EXPB rExp rExpS))
							(if (and (eql LO nil) (eql RO nil))
								(let ()
									(setq lIndex (divideEXPI nExpTo))

									(if (/= lIndex 0) (setq rIndex (divideEXPI (subseq nExpTo lIndex))))

									(if (and (/= lIndex 0) (/= rIndex 0) (= (+ 3 lIndex rIndex) len))
										(let ()
											(setq lExp (subseq nExpTo 0 lIndex))
											(setq rExp (subseq nExpTo lIndex))

											(setq lExpS (subseq nExpS  0 lIndex))
											(setq rExpS (subseq nExpS lIndex))

											(setq LO (EXPI lExp lExpS))
											(setq RO (EXPI rExp rExpS))
										)
										(return-from EXPB nil)
									)
								)
							)

							(if (and (not (eql LO nil)) (not (eql RO nil))  )
								(return-from EXPB (equal_ LO RO) )
								(return-from EXPB nil)
							)

						)

						(return-from EXPB nil)
					)
				)
			)

			;; Keyword not
			( (and (string= (first ExpTo) "OP_OP") (string= (car (last ExpTo)) "OP_CP") (string= (second ExpTo) "KW_NOT") ) 
				(prog ((LO) (RO) (lIndex) (rIndex) (lExp) (rExp) (nExpTo)  (nExpS) (lExpS) (rExpS))

					(setq nExpTo (subseq ExpTo 2 (- len 1)))
					(setq nExpS (subseq ExpS 2 (- len 1)))
					(setq lIndex (divideEXPB nExpTo))

					(if (and (/= lIndex 0) (= (+ 3 lIndex) len))
						(prog ()
							(setq lExp (subseq nExpTo 0 lIndex))
							(setq lExpS (subseq nExpS  0 lIndex))
							(setq LO (EXPB lExp lExpS))

							(if (not (eql LO nil))
								(return-from EXPB (not_ LO) )
								(return-from EXPB nil)
							)

						)

						(return-from EXPB nil)
					)
				)
			)

			(t (return-from EXPB nil))
		)
	)

)

(defun EXPLISTI(ExpTo ExpS)
	(prog ( (len (list-length ExpTo)) (tempList))
		(cond
			;; null

			( (and (= len 1) (string= (first ExpTo) "KW_NIL")) 

				(return nil)
			)
			;; list value
			( (not(equal (setq tempList (LISTVALUE ExpTo ExpS)) -1))  

				(return tempList)
			)
			
			;; append
			( (and (string= (first ExpTo) "OP_OP") (string= (car (last ExpTo)) "OP_CP") (string= (second ExpTo) "KW_APPEND") ) 
				(prog ((LO) (RO) (lIndex) (rIndex) (lExp) (rExp) (nExpTo)  (nExpS) (lExpS) (rExpS))

					(setq nExpTo (subseq ExpTo 2 (- len 1)))
					(setq nExpS (subseq ExpS 2 (- len 1)))
					(setq lIndex (divideEXPI nExpTo))

					(if (and (/= lIndex 0))
						(prog ()
							(setq lExp (subseq nExpTo 0 lIndex))
							(setq rExp (subseq nExpTo lIndex))

							(setq lExpS (subseq nExpS  0 lIndex))
							(setq rExpS (subseq nExpS lIndex))

							(setq LO (EXPI lExp lExpS))
							(setq RO (EXPLISTI rExp rExpS))

							(if (and (not (eql LO nil)) (not (equal RO -1))  )
								(return-from EXPLISTI (cons LO RO) )
								(return-from EXPLISTI -1)
							)

						)

						(return-from EXPLISTI -1)
					)
				)
			)

			;; concat
			( (and (string= (first ExpTo) "OP_OP") (string= (car (last ExpTo)) "OP_CP") (string= (second ExpTo) "KW_CONCAT") ) 
				(prog ((LO) (RO) (lIndex) (rIndex) (lExp) (rExp) (nExpTo)  (nExpS) (lExpS) (rExpS))

					(setq nExpTo (subseq ExpTo 2 (- len 1)))
					(setq nExpS (subseq ExpS 2 (- len 1)))
					(setq lIndex (divideEXPLISTI nExpTo))

					(if (/= lIndex 0) (setq rIndex (divideEXPLISTI (subseq nExpTo lIndex))) )

					(if (and (/= lIndex 0) (/= rIndex 0) (= (+ 3 lIndex rIndex) len))
						(prog ()
							(setq lExp (subseq nExpTo 0 lIndex))
							(setq rExp (subseq nExpTo lIndex))

							(setq lExpS (subseq nExpS  0 lIndex))
							(setq rExpS (subseq nExpS lIndex))

							(setq LO (EXPLISTI lExp lExpS))
							(setq RO (EXPLISTI rExp rExpS))
							
							(if (and (not (eql LO -1)) (not (eql RO -1))  )
								(return-from EXPLISTI (append LO RO) )
								(return-from EXPLISTI -1)
							)

						)

						(return-from EXPLISTI -1)
					)
				)
			)

			;; if EXB EXPLISTI
			( (and (> len 4) (string= (first ExpTo) "OP_OP") (string= (car (last ExpTo)) "OP_CP") (string= (second ExpTo) "KW_IF") ) 
				(prog ((LO) (RO) (lIndex) (rIndex) (lExp) (rExp) (nExpTo)  (nExpS) (lExpS) (rExpS))

					(setq nExpTo (subseq ExpTo 2 (- len 1)))
					(setq nExpS (subseq ExpS 2 (- len 1)))
					(setq lIndex (divideEXPB nExpTo))

					(if (/= lIndex 0) (setq rIndex (divideEXPLISTI (subseq nExpTo lIndex))))

					(if (and (/= lIndex 0) (/= rIndex 0) (= (+ 3 lIndex rIndex) len))
						(prog ()
							(setq lExp (subseq nExpTo 0 lIndex))
							(setq rExp (subseq nExpTo lIndex))

							(setq lExpS (subseq nExpS  0 lIndex))
							(setq rExpS (subseq nExpS lIndex))

							(setq LO (EXPB lExp lExpS))
							(setq RO (EXPLISTI rExp rExpS))

							(if (and (not (eql LO nil)) (not (eql RO -1))  )
								(if (= LO 1) 
									(return-from EXPLISTI RO)
									(return-from EXPLISTI 0)
								)
								(return-from EXPLISTI -1)
							)

						)

						(return-from EXPLISTI -1)
					)
				)
			)
			

			(t (return-from EXPLISTI -1))
		)
	)
)


(defun LISTVALUE(ExpTo ExpS)
	(prog ( (len (list-length ExpTo)) )
		(cond
			;; null

			( (and (= len 1) (string= (first ExpTo) "KW_NIL")) 

				(return nil)
			)

			( (and (= len 3) (string= (first ExpTo) "OP_OP") (string= (car (last ExpTo)) "OP_CP") (string= (second ExpTo) "KW_LIST")) 

				(return nil)
			)

			;; Keyword and
			( (and  (> len 3) (string= (first ExpTo) "OP_OP") (string= (car (last ExpTo)) "OP_CP") (string= (second ExpTo) "KW_LIST") ) 
				(prog ( (index 0) (nExpTo) (nExpS) (tempList))

					(setq nExpTo (subseq ExpTo 2 (- len 1)))
					(setq nExpS (subseq ExpS 2 (- len 1)))
					

					(loop for temp in nExpTo
				  		do
				  			(if (string= temp "VALUE") 
				  				(push (parse-integer (nth index nExpS)) tempList)
				  				(return-from LISTVALUE -1)
				  			)
				  			(setf index (+ index 1))
					)
					(return-from LISTVALUE (reverse tempList))

				)
			)

			(t (return-from LISTVALUE -1))
		)
	)
)


(defun varValue(ID)
	(prog ((index 0))
		(loop for temp in varList
	  		do
	  			(if (string= temp ID) (return-from varValue index))

		  		(setf index (+ 1 index))
		)
		(push ID varList)
		(push 50 varValues)
		(return 0)
	)	
)

(defun divideEXPI(ExpTo)
	(prog ((index 0) (PC 0))
		(cond
			( (or (string= (car ExpTo) "VALUE") (string= (car ExpTo) "IDENTIFIER")) (return-from divideEXPI 1 ) )

			( (string= (car ExpTo) "OP_OP") 
				(prog ()
					(loop for temp in ExpTo
				  		do
				  			(setf index (+ index 1))
				  			(if (string= temp "OP_OP") (setf PC (+ PC 1)))
				  			(if (string= temp "OP_CP") (setf PC (- PC 1)))
				  			(if (= PC 0) (return-from divideEXPI index))
					)
					(return-from divideEXPI 0)
				)	
			)
			( t (return-from divideEXPI 0))

		)
	)	
)

(defun divideEXPB(ExpTo)
	(prog ((index 0) (PC 0))
		(cond
			( (or (string= (car ExpTo) "VALUE") (string= (car ExpTo) "KW_TRUE") (string= (car ExpTo) "KW_FALSE")) (return-from divideEXPB 1 ) )

			( (string= (car ExpTo) "OP_OP") 
				(prog ()
					(loop for temp in ExpTo
				  		do
				  			(setf index (+ index 1))
				  			(if (string= temp "OP_OP") (setf PC (+ PC 1)))
				  			(if (string= temp "OP_CP") (setf PC (- PC 1)))
				  			(if (= PC 0) (return-from divideEXPB index))
					)
					(return-from divideEXPB 0)
				)	
			)
			( t (return-from divideEXPB 0))

		)
	)	
)

(defun divideEXPLISTI(ExpTo)
	(prog ((index 0) (PC 0))
		(cond
			( (string= (car ExpTo) "KW_NIL") (return-from divideEXPLISTI 1 ) )

			( (string= (car ExpTo) "OP_OP") 
				(prog ()
					(loop for temp in ExpTo
				  		do
				  			(setf index (+ index 1))
				  			(if (string= temp "OP_OP") (setf PC (+ PC 1)))
				  			(if (string= temp "OP_CP") (setf PC (- PC 1)))
				  			(if (= PC 0) (return-from divideEXPLISTI index))
					)
					(return-from divideEXPLISTI 0)
				)	
			)
			( t (return-from divideEXPLISTI 0))

		)
	)	
)


(defun INPUT ()
	(prog (value)
		(if (eql (setq value (EXPI tokens tokenString)) nil)

			(if (not (eql (setq value (EXPLISTI tokens tokenString)) -1))
				(return (list value 1))

				(return (list value -1))
			)

			(return (list value 1))
		)
	)	

)

(defun and_(L R)
	(if (and (= L 1) (= R 1)) 1 0)
)


(defun or_(L R)
	(if (or (= L 1) (= R 1)) 1 0)

)

(defun not_(L)
	(if (= L 1) 0 1)

)

(defun equal_(L R)
	(if (= L R) 1 0)

)

(gppinterpreter)

