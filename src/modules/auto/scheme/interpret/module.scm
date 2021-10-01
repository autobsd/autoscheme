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
	  )


  (begin
    ;; ((environment-ref (global-environment) (string->symbol "load-modules")))

(    (foreign-function ff_load_modules)
)


    ;; (define interpretation-environment
    ;;   (lambda ()


    ;; 	(let ((env (environment (only (auto scheme) import define-library))))
    ;; 	  (for-each (lambda (sym)
    ;; 		      (let ((binding (environment-assoc (global-environment) sym)))
    ;; 			(if (environment? (cdr binding))
    ;; 			    (environment-define! env (car binding) (cdr binding)))))
		    
    ;; 		    (environment-defined-symbols (global-environment)))
    ;; 	  env)))

    
    (define interpret
      (lambda sources

	(display "sources: ")(write sources)(newline)


	;; (display "interpretation symbols: ")(write (environment-defined-symbols (interpretation-environment)))(newline)  

	))

    ))