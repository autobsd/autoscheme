;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-define (include-string "definitions.c"))


(define-library (scheme process-context)

  (import (auto scheme base)
	  )

  (export command-line
	  emergency-exit
	  exit

	  get-environment-variables
	  get-environment-variable
	  )
  

  (begin

    (define command-line (foreign-function ff_command_line))
    (define get-environment-variables (foreign-function ff_get_environment_variables))

    (define get-environment-variable 
      (lambda (name)
	(let ((variable (assoc name (get-environment-variables))))
	  (and variable (cdr variable)))))

    (define emergency-exit (foreign-procedure LOC_EMERGENCY_EXIT)) 


    (define exit #f)
    (call-with-current-continuation 
     (lambda (return)
       (let ((obj (call-with-current-continuation 
    		   (lambda (_exit)
    		     (cond ((not exit)
    			    (set! exit _exit)
    			    (return)
    			    )
    			   )
    		     ))))

    	 (emergency-exit (cond ((integer? obj) obj)
    			       ((eq? obj #f) 1)
    			       (else 0)))
	 
    	 )
       )
     )

    

    )
  )


