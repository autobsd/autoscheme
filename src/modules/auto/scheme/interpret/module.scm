(define-library (auto scheme interpret)
  
  (export interpretive-environment interpret)

  (begin
    
    (define interpretive-environment
      (lambda ()
	(let ((env (make-environment)))

	  (environment-import! env 
			       (environment-ref (current-environment) (symbol "(auto scheme environment)")))

	  (for-each (lambda (sym) 

	  	      (cond ((equal? ((symbol->string sym) 0) #\()
			     
	  		     (environment-update! env sym (environment-ref (current-environment) sym))
			     
	  		     )
	  		    )
	  	      )
	  	    (environment-defined (current-environment)))


	  env)
	))

    (define interpret
      (lambda (source-files)


	(for-each (lambda (file)
		    (load file (interpretive-environment))
		    )
		  source-files)
	
	
	))
    ))


