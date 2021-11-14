;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare "foreign_function ff_load_modules;")


(define-library (scheme eval)

  (import (only (auto scheme base) 

	  	define
	  	cons
	  	quasiquote
	  	quote
	  	append
  	  	)
	  )

  (export environment eval)

  (begin
    (define eval (foreign-operation LOC_PEVAL))
    (define define-macro (foreign-syntax LOC_DEFMACRO0 "define-macro"))
    (define load-modules (foreign-function ff_load_modules))
    (define make-environment (foreign-function make_environment))

    (define-macro (environment . sets) 
      (define _environment (load-modules (make-environment)))

      (eval `(import ,@sets) _environment)
      _environment)
    
    )
  )


