(define-library (auto srfi 13)

  (export string-join)

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

    ))