

;; to construct huffman tree
(defstruct node

	ch
	freq
	left
	right
)

;; insert the item into the list as descending order
(defun insert-min (lst item)
	(insert-min-helper lst item (list ))
)

;;helper for insert-min method
(defun insert-min-helper(lst item exlst)
	(cond
		( (eql (car lst) nil) (reverse (cons item exlst)) )

		( (< (node-freq item) (node-freq (car lst)) ) (append (reverse exlst) (cons item lst)) )

		( t (insert-min-helper (cdr lst) item (cons (car lst) exlst)) )

	)
)

;; read text at the paragraph and construct a list like ((e 26) (k 5))
;; (character frequency) list for each character and return it
(defun read_from_file ()

  (prog (freqlist)

    (with-open-file (stream "paragraph.txt":direction :input)
      
      (setq freqlist (list))    
      (setf freqlist (fillup stream freqlist) )
    )
    (return freqlist)
  )
)

;; helper function for reading from file and construct (character frequency) list
(defun fillup (stream freqlist)

  (let (inp)

    (if (setq inp (read-char stream nil nil nil))

      ( let()
        
        (setf freqlist (check_add inp freqlist))

        (fillup stream freqlist)
      )

      (return-from fillup freqlist)
    )
  )
)

;; helper function reading from file and construct (character frequency) 
;; its aim is check character and according to it increment frequency or add it to list
(defun check_add(inp freqlist)

  (if (not (check_list inp freqlist))
    
   (cons (make-node :ch inp :freq 1) freqlist)
   (return-from check_add freqlist)

  )
  
)

;; helper function reading from file and construct (character frequency) 
;; its aim is check character is readed befor or not
(defun check_list(inp freqlist)

  (cond
    ( (eql freqlist nil) nil)
    ( (char= (node-ch (first freqlist)) inp) (setf (node-freq (first freqlist)) (+ 1 (node-freq (first freqlist)))) )
    ( (not(char= (node-ch (first freqlist)) inp)) (check_list inp (cdr freqlist)))
  )
)

;; for sort function
(defun comp (n)
	(node-freq n)
)


;; construct the huffman tree
(defun huffman (lst)

	(cond

		( (eql (cdr lst) nil) (return-from huffman (car lst)))
		( t 

		  (let (n1 n2 newNode)
		  	
		  	(setq n1 (first lst))
		  	(setq n2 (second lst))

		  	(setq newNode (make-node :freq (+ (node-freq n1) (node-freq n2)) :left n1 :right n2))
		  	
		  	(huffman (insert-min (cddr lst) newNode))
		  )

		)
	)
)

;;print huffman tree level by level
(defun print-level-order(root)

  (let (que)
    (with-open-file (stream "huffman_codes.txt":direction :output)
      (if (eql root nil)
        ()
        (print-all-levels (list (list root nil)) stream)
      )
    )
  )

)

;; helper for printing huffman tree
(defun print-all-levels(que stream)
  
  (let (size)
    (setq size (length que))

    (if (> size 0)
      (let ()
        (setf que (print-level que size stream))
        (print-all-levels que stream)
      )

      ()
    )
    
  )

)


;; helper for printing huffman tree
(defun print-level(que size stream)

  (if (> size 0)

    (let (Tnode)
      (setq Tnode (car (last que)))
      (setq que (butlast que))

      (if (eql (node-ch (car Tnode)) nil)
        ()
        (let () 

          (format stream "~c : " (node-ch (car Tnode)))
          (write-line (second Tnode) stream)
        )
        
      )

      (if (eql (node-left (car Tnode)) nil)
        ()
        (let (code) 
          (setq code (concatenate 'string (second Tnode) "0"))
          (setf que (cons (list (node-left (car Tnode)) code) que))
        )
      )

      (if (eql (node-right (car Tnode)) nil)
        ()
        (let (code) 
          (setq code (concatenate 'string (second Tnode) "1"))
          (setf que (cons (list (node-right (car Tnode)) code) que))
        )
      )

      (print-level que (- size 1) stream)

    )

    que

  )
)

;; tests the program
(defun test()
  (setq lst (read_from_file ))

  (sort lst #'< :key #'comp)

  (print-level-order (huffman lst))
)

(test)
