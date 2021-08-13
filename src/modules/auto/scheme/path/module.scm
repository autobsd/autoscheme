(define-library (auto scheme path)

  (export path-directory
	  path-absolute?
	  path-make-absolute
	  )

  (begin 

    (define path-directory
      (lambda (path)
	(let ((pos -1)
	      )
	  (do ((len (length path))
	       (i 0 (+ i 1)))
	      ((= i len))
	    (if (char=? (path i) #\/)
		(set! pos i)))

	  (if (positive? pos) 
	      (substring path 0 pos)))))


    (define path-absolute?
      (lambda (path)
	(char=? (path 0) #\/)))


    (define path-make-absolute
      (lambda (path . rest)
	(if (path-absolute? path) 
	    path
	    (let ((parent-dir (if (pair? rest) 
				  (car rest)
				  (current-directory)))
		  )
	      (string-append parent-dir "/" path)))))
    
    ))