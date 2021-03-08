
;;reads nested list from the file and return it

(defun read_nested_list() 
	
	(prog ((text) (nested_list))

		( setq text ( concatenate 'string "(" ( read-line (open "nested_list.txt") ) ")" ) )
	 	( setq nested_list (read-from-string text) )

		(return nested_list)
	 )	 
)

;;takes nested list and parse it each time
;;append to one list and return it 

(defun flatten (main_list)
    (if (eql main_list nil)
        
        nil

        (let ((elem (car main_list)) (sub_list (cdr main_list)))
            (if (listp elem) 
            	(append (flatten elem) (flatten sub_list)) 
            	(append (cons elem nil) (flatten sub_list ))
            )
        )
    )
)

;writes flattened list to file

(defun writing_file (filename flattened_list)

	(with-open-file (stream filename :direction :output)
	   (write-line (string-trim "()" (write-to-string flattened_list)) stream)
	)	
)


(defun flattener()
	
	(setq nested_list (read_nested_list))

	(writing_file (string "flattened_list.txt") (flatten nested_list))

)


(flattener)