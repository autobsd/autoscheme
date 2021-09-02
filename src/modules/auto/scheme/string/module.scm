;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme string)

  (import (only (s7) list->string string->list))

  (export string-join
	  string-prefix? 
	  string-map 
	  string-tokenize
	  )

  (begin 

    (define string-join
      (lambda (string-list . rest)

	(let ((delimeter (if (pair? rest) (car rest) " "))
	      (grammar (if (and (pair? rest)(pair? (cdr rest))) (cadr rest) 'infix))
	      )

	  (if (and (equal? grammar 'strict-infix) (null? string-list)) (error "cannot join with 'strict-infix" string-list))

	  (let join-strings ((remainder string-list)
			     )
	    (cond ((null? remainder) "")
		  ((equal? grammar 'suffix) (string-append (car remainder) delimeter (join-strings (cdr remainder))))
		  ((equal? grammar 'prefix) (string-append delimeter (car remainder) (join-strings (cdr remainder))))

		  ((pair? (cdr remainder)) (string-append (car remainder) delimeter (join-strings (cdr remainder))))
		  (else (car remainder)))))))


    
    (define string-prefix?
      (lambda (s1 s2 . rest)

    	(let ((start1 (or (and (pair? rest) 
			       (car rest)) 
			  0))

    	      (end1 (or (and (pair? rest)
    			     (pair? (cdr rest))
			     (cadr rest))
			(string-length s1)))

    	      (start2 (or (and (pair? rest)
    			       (pair? (cdr rest))
    			       (pair? (cddr rest))
			       (caddr rest))
			  0))

    	      (end2 (or (and (pair? rest)
			     (pair? (cdr rest))
			     (pair? (cddr rest))
			     (pair? (cdddr rest))
			     (cadddr rest))
			(string-length s2)))
    	      )

	  (equal? (substring s1 start1 end1) 
		  (substring s2 start2 end2)))))
    


    (define string-map
      (lambda (proc . strings)

	(let sub-char ((pos 0))

	  (let* ((char-list (call/cc (lambda (return)
				       (map (lambda (s)
					      (if (< pos (string-length s)) 
						  (string-ref s pos) 
						  (return #f)))
					    strings))))
		 (result (if char-list 
			     (apply proc char-list)
			     #f))

		 (result-string (if (char? result) 
				    (string result) 
				    result))
		 )

	    (if char-list 
		(string-append result-string (sub-char (+ pos 1)))
		"")))))



    (define string-tokenize
      (lambda (s)

	(let get-tokens ((current '())
			 (remainder (string->list s))
			 )

	  (cond ((null? remainder) (list (list->string (reverse current))))

		((equal? (car remainder) #\space) (cons (list->string (reverse current)) (get-tokens '() (cdr remainder))))

		(else (get-tokens (cons (car remainder) current) (cdr remainder) ))))
	))
    


    ))