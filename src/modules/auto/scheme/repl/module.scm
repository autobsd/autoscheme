;; (scheme repl)
(define-library (auto scheme repl)
  (export interaction-environment)
  (import (s7))
  (begin 
    (define interaction-environment
      (lambda ()
	  (let ((env (inlet 'import import
				  
			    (symbol (object->string '(s7)))
			    (symbol->value (symbol (object->string '(s7))))

			    )
		     )
		)
	    env
	    )
	  ))))