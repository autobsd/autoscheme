(define-library (auto scheme interpret)

  (export interpret)
  
  (begin
    
    (define interpret
      (lambda (source-files)

	(let ((env (make-environment)))


	  (environment-import! env 
			       (list (symbol->value (symbol (object->string '(auto scheme environment))))))

	  ;; (for-each (lambda (l)
	  ;; 	      (environment-update! env
	  ;; 				   (symbol (object->string l))
	  ;; 				   (symbol->value (symbol (object->string l)))))
	  ;; 	    '((auto scheme write))
	  ;; 	    )

	  (environment-update! env (symbol (object->string '(auto scheme write))) (symbol->value (symbol (object->string '(auto scheme write)))))

	  (display "---->: ")(write (let->list (symbol->value (symbol (object->string '(auto scheme repl))))))(newline)
	  (exit)
	  ;; (vector-for-each (lambda (sym) 

	  ;; 		     (cond ((equal? ((symbol->string sym) 0) #\()
	  ;; 			    (display sym)(newline)
				    
	  ;; 			    ;; (environment-update! env sym (symbol->value sym))
				    
	  ;; 			    )
	  ;; 			   )
	  ;; 		     )
	  ;; 		   (symbol-table))


	  (display "env: ")(write (let->list env))(newline)


	  (for-each (lambda (file)
		      (load file env))
		    source-files)
	  
	  )

	))
    ))


