(define-library (auto scheme interpret)
  
  (export interpret)
  (begin
    
    (define interpret
      (lambda (source-files)

	(let ((env (make-environment)))

	  (environment-import! env 
			       (list (environment-ref (current-environment) (symbol "(auto scheme environment)"))))

	  (environment-update! env '_list _list)

	  (for-each (lambda (sym) 

	  	      (cond ((equal? ((symbol->string sym) 0) #\()
	  		     ;; (display sym)(newline)
			     
	  		     (environment-update! env sym (environment-ref (current-environment) sym))
			     
	  		     )
	  		    )
	  	      )
	  	    (environment-defined (current-environment)))

	  ;; (display "env: ")(write (environment-defined env))(newline)

	  (for-each (lambda (file)
	  	      (load file env)
	  	      )
	  	    source-files)
	  
	  )
	))
    ))


