
(defvar facts (list))
(defvar predicates (list))
(defvar querrys (list))

(defun read-file (filename)
  (with-open-file (instream filename :direction :input :if-does-not-exist nil)
    (when instream 
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))


(defun split-list (l)
	(let ((element) (res))

		(setq element (car l))
		(setq res (cdr l))

		(loop while (not (equal element nil))
			do

			(cond
				((equal (car element) nil) (push element querrys))
				((equal (car (last element)) nil) (push element facts))
				(t (push element predicates))
			)
			(setq element (car res))
			(setq res (cdr res))

		)
		(setq querrys (reverse querrys))

	)

)

(defun isVariable(str)
	(if (and (stringp str) (< 64 (char-code (char str 0))) (< (char-code (char str 0)) 91) )
		t
		nil
	)

)

(defun isObject(str)
	(if (alpha-char-p (char str 0))
		(not (isVariable str))
		nil
	)
)

(defun querry (ql)
	(let ()
		(if (isFact ql)
			(return-from querry t)
			(return-from querry (isPredicate ql) )
		)
	)

)

(defun querryAll ()
	(let( (index 0) )

		(with-open-file (ostream "output.txt" :direction :output)
			(loop while (< index (list-length querrys))
			do
			(if (querry (cadr (nth index querrys)))
				(write-line "true" ostream )
				(write-line "false" ostream )
			)
			
			(setq index (+ 1 index))
		)		
		)
		
	)

)

(defun isFact(ql)
	(let ((tempF) (res))

		(setq tempF (car (car facts)))
		(setq res (cdr facts))

		(loop while (not (equal tempF nil))
			do
			(if (and (equal (car tempF) (car ql)) (equal (list-length (cadr tempF)) (list-length (cadr ql))) ) ; names and lengths are equal

				(let ((varList1 (car (cdr tempF))) (varList2 (car (cdr ql))) (index 0) (len))

					(setq len (list-length varList1))
					(loop while (< index len)
						do
						(if (isVariable (nth index varList1)) (setq varList1 (subst (nth index  varList2) (nth index  varList1) varList1 :test #'equal))) 
						
						(setq index (+ 1 index))
					)

					(if (equal varList1 varList2) (return-from isFact t) )
				)
				
				nil
			)
			(setq tempF (caar res))
			(setq res (cdr res))
		)
		(return-from isFact nil)

	)

)

(defun isPredicate(ql)
	(let ((tempF) (res) (tempB))

		(setq tempF (car (car predicates)))
		(setq tempB (car (cdr (car predicates))))
		(setq res (cdr predicates))

		(loop while (not (equal tempF nil))
			do
			(if (and (equal (car tempF) (car ql)) (equal (list-length (cadr tempF)) (list-length (cadr ql))) ) ; names and lengths are equal

				(let ((varList1 (car (cdr tempF))) (varList2 (car (cdr ql))) (index 0) (len))

					(setq len (list-length varList1))
					(loop while (< index len)
						do
						(if (isVariable (nth index varList1))
							(let()
								(setq tempB (subst (nth index  varList2) (nth index  varList1) tempB :test #'equal))
								(setq varList1 (subst (nth index  varList2) (nth index  varList1) varList1 :test #'equal))
							)
						) 
						
						(setq index (+ 1 index))
					)

					(if (equal varList1 varList2) 
						(let ((subP) (resB) (result t))
							(setq subP (car tempB))
							(setq resB (cdr tempB))
							(loop while (and (not (equal subP nil)) result)
								do
								(if (querry subP)
									(setq result t)
									(setq result nil)
								)

								(setq subP (car resB))
								(setq resB (cdr resB))

							)
							(if result 
								(return-from isPredicate t)
								(return-from isPredicate nil)
							)
						) 
					)
				)
				
				nil
			)
			(setq tempF (caar res))
			(setq tempB (car (cdr (car res))))
			(setq res (cdr res))
		)
		(return-from isPredicate nil)
	)

)

(setq li (read-from-string (read-file "input.txt")))

(split-list li)


(querryAll)
