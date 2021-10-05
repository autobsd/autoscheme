;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-define (include-string "definitions.c"))



(define-library (auto scheme interpret)

  (import (auto scheme base)
	  (auto scheme write)
	  (auto scheme environment)
	  (auto scheme eval)
	  )

  (export interpret
	  interpretation-environment
	  )


  (begin

    (define interpretation-environment
      (lambda ()
	(let ((load-modules (foreign-function ff_load_modules))
	      (int-env (make-environment))
	      )
	  
	  (load-modules int-env)
	)))

     
     (define interpret
       (lambda sources

	 (display "sources: ")(write sources)(newline)
	 (define int-env (interpretation-environment))
	 (display "int-env: ")(write int-env)(newline)


	 (display "interpretation symbols: ")(write (environment-defined-symbols int-env))(newline)  

	 (eval '(begin (import (only (auto scheme base) newline)
			       (auto scheme write)) 
		       (display "hello world")
		       (newline))
	        int-env)

	 ))

     ))