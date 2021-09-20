;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-macro (define-library name . declarations)
  (let ((name-symbol (string->symbol (object->string name)))
	(quoted-declarations (map (lambda (declaration)
				    (_quasiquote ',declaration)
				    )
				  declarations))
	)

    (apply environment-define! (cons (calling-environment) (cons name-symbol (list (apply make-library declarations)))))
    ;; (apply environment-define! (cons (global-environment) (cons name-symbol (list (apply make-library declarations)))))

    ))


(define-macro (import . sets)

  (cond (else;;(and (pair? sets)(pair? (car sets))(equal? (caar sets) 'except)) 
  	 (display "found import sets: ")(write sets)(newline)
  	 (display "calling-environment: ")(write (environment-defined-symbols (calling-environment)))(newline)
  	 (display "length: ")(write (length (calling-environment)))(newline)
	 (newline)
  	 ;; (display "current-environment: ")(write (environment-defined-symbols (current-environment)))(newline)
  	 ;; (display "length: ")(write (length (current-environment)))(newline)
	 ;; (newline)
  	 (display "global-environment: ")(write (environment-defined-symbols (global-environment)))(newline)
  	 (display "length: ")(write (length (global-environment)))(newline)
  	 ))

  ;; (apply environment-import-sets! (cons (global-environment) sets))
  (apply environment-import-sets! (cons (calling-environment) sets))

  )


