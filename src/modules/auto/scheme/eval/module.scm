;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme eval)

  (import (only (auto scheme) 
  	        begin
  		define-macro
  		lambda
		eval
		cons
		apply
  	  ;; 	)

	  ;; (only (auto scheme environment)
		make-environment
		;; )

	  ;; (only (auto scheme library)
		environment-import-sets!
		)
	  )

  (export environment eval)
  (begin

    (define-macro (environment . sets) 

      (apply environment-import-sets! (cons (make-environment) (cons (expansion-environment) sets)))
      
      )

    ))


