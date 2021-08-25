(define-library (auto scheme repl)

  (export interaction-environment repl)

  (import (only (s7) catch))

  (begin 
    
    (define interaction-environment
      (lambda ()
	(let ((env (make-environment)))

	  (for-each (lambda (sym) 

	  	      (cond ((equal? ((symbol->string sym) 0) #\()
			     
	  		     (environment-update! env sym (environment-ref (current-environment) sym))
			     
	  		     )
	  		    )
	  	      )
	  	    (environment-defined (current-environment)))

	  (for-each (lambda (sym) 
		      (let ((sym-str (symbol->string sym)))

			(cond ((and (char=? (sym-str 0) #\() (not (string=? sym-str "(s7)")))

			       (environment-import! env 
						    (environment-ref (current-environment) sym))
			       )
			      )
			))
	  	    (environment-defined env))
	  env)
	))


    (define repl 
      (lambda (env)
	(display "scheme-REPL> ")
	(catch #t 
	       (lambda ()
		 (write (eval (read) env))
		 )
	       (lambda (error-type message . rest)
		 (cond ((equal? error-type 'unbound-variable)

			(display "Evaluation error - " (current-error-port))
			(format (current-error-port) (apply values message))
			)

		       (else (display "Error - ")(for-each (lambda (line) (display line)(newline)) message)
			     (display "error-type: ")(write error-type)(newline)
			     (display "message: ")(write message)(newline)
			     (display "rest: ")(write rest)(newline)
			     
			     ))
		 )
	       ;; (lambda args
	       ;;   (format (current-error-port) "~A: ~A~%~A[~A]:~%~A~%" 
	       ;; 	     (car args)                        ; the error type
	       ;; 	     (apply format #f (cadr args))     ; the error info
	       ;; 	     (port-filename) (port-line-number); error file location
	       ;; 	     (stacktrace)                      ; and a stacktrace
	       ;; 	     ))
	       )
	
	(newline)
	(repl env)
	))

    ))