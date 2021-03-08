

;;chechk n is prime or semiprime
;;if it is prime returns 1
;;if it is semirpime returns 2
;;if it is no prime or semiprime returns 3
(defun is_prime_semiprime (n)
	(let ((div_list) (div_count))
		(cond 
			((< n 1) (return-from is_prime_semiprime nil))
			(t (setq div_count (list-length (setq div_list (find_divisors n (floor n 2)))) ) )
		)

		(cond
			((= 0 div_count) 1) ;; prime
			((< 2 div_count) 3) ;; nor semiprime or prime
			((= 1 div_count) 2) ;; semiprime	
			((= 2 div_count) (if (and (= 1 (is_prime_semiprime (car div_list))) (= 1 (is_prime_semiprime (car (cdr div_list)))) ) 2 3) ) ;; semiprime

		)
	)	
)

;; find divisors of n and return it as a list

(defun find_divisors(n m)
  	(cond
		((<= m 1) ())
		((= 0 (mod n m)) (cons m (find_divisors n (- m 1))))
		(t (find_divisors n (- m 1)))
  	)
)

;;reads boundries and return it as a list  like (17 65)

(defun read_boundries() 
	
	(prog ((text) (boundries_list))

		( setq text ( concatenate 'string "(" ( read-line (open "boundries.txt") ) ")" ) )
	 	( setq boundries_list (read-from-string text) )
		
		(return boundries_list)
	)  
)

;;it writes to file each prime and semi-prime number between low_bound and up_bound 

(defun writing_file (stream low_bound up_bound)

	(cond
		((> low_bound up_bound) (return-from writing_file 0))
		( (= 1 (is_prime_semiprime low_bound)) (write-line (concatenate 'string (write-to-string low_bound) " is Prime") stream) )
		( (= 2 (is_prime_semiprime low_bound)) (write-line (concatenate 'string (write-to-string low_bound) " is Semi-prime") stream) )

	)	

	(writing_file stream (+ 1 low_bound) up_bound)
)

;; it reads boundries and writes to file each prime and semi-prime number between boundries

(defun primecrawler()

	(let ((b_list))

	 	( setq b_list (read_boundries) )

 		(with-open-file (stream "primedistribution.txt":direction :output)
			
			(writing_file stream (car b_list) (car(cdr b_list)) ) 

		)
	)	 
)

(primecrawler)