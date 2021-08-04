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