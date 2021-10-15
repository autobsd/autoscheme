;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme eval)

  (import (only (auto scheme base) 
  		define-macro
		eval
		cons
		apply
  	  	)

	  (only (auto scheme environment)
		make-environment
		global-environment
		)

	  (only (auto scheme library)
		environment-import-sets!
		)

	  )

  (export environment eval)
  (begin

    (define-macro (environment . sets) 

      (apply environment-import-sets! (cons (make-environment) (cons (expansion-environment) sets)))
      ;; (apply environment-import-sets! (cons (make-environment) (cons (global-environment) sets)))
      
      )

    ))


