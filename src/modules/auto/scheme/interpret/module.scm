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
	  (auto scheme file)
	  (auto scheme read)
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

	 (define int-env (interpretation-environment))
	 (environment-delete! int-env 'let)
	 (environment-delete! int-env 'begin)

	 (for-each (lambda (source)
		     (with-input-from-file 
		      source
		      (lambda ()
			(let interpret-expression ((expression (read)))
			  (cond ((not (eof-object? expression))
				 (eval expression int-env)
				 (interpret-expression (read)))))
			)
		      ))
		   sources)
	 ))

     ))