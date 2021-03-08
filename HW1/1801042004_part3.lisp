
;; read at most 5 integer from file and return it as a list
(defun read_inputs(stream) 
	
	(prog ((inputs) (elem))
		
		(setq inputs nil)

		(dotimes (n 5)

			(if  (setq elem(read-preserving-whitespace stream nil nil nil))

				
				(setq inputs (cons elem inputs))
				(return (reverse inputs))
			)
			
		)
		(return (reverse inputs))
	)  
)

;; return collatz sequence number of n as a list
(defun collatz_seq(n)
	(cond
		((= 1 n) (cons 1 nil))
		((= 0 (mod n 2)) (cons n (collatz_seq (/ n 2))))
		((= 1 (mod n 2)) (cons n (collatz_seq (+ 1 (* n 3)))))
	)
)

;; writing each inpusts with their collatz sequence 
(defun writing_file(stream inputs)

    (if (eql (car inputs) nil) ()

		(let ((next_value))
			(setq next_value (car inputs))
			(write-line (concatenate 'string (write-to-string next_value) ": " (string-trim "()" (write-to-string (collatz_seq next_value))) ) stream)
			(writing_file stream (cdr inputs))
		
		)
	)
)

;;tests program
(defun test ()
	(let (inputs) 

		(with-open-file (stream "integer_inputs.txt":direction :input)
				
			(setq inputs (read_inputs stream))
		)

		(with-open-file (stream "collatz_outputs.txt":direction :output)
				
			(writing_file stream inputs)
		)

	)	
)

(test)

;(print (collatz_seq 17))