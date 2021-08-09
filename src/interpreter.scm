(define interpret-program
  (lambda (source-files)
    (display "interpetting program from: ")(write source-files)(newline)
    ;; (with-let (inlet 'source-files source-files)
	      ;; (for-each load source-files)
	      ;; )
    (let ((interaction-env (inlet
			    'environment-import! environment-import!
			    'current-environment current-environment

			    'import import
				  
				  (symbol (object->string '(auto scheme write)))
				  (symbol->value (symbol (object->string '(auto scheme write))))

				  )))
      (load (car source-files) interaction-env)
      )
    (exit)
    ))