;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (only (auto scheme) 
	      display
	      newline
	      write
	      quit
	      let
	      define
	      apply
	      cons
	      quote
	      string->symbol 
	      )
	(auto scheme environment)
	)
(display "inside AutoScheme application...\n")

(define x 7)
(display "current-environment: ")(write (environment-defined-symbols (current-environment)))(newline)
(display "x: ")(write x)(newline)
(newline)

(let ()
  (import (only (auto scheme environment)
		environment-update!
		environment-ref
		))
  (define x 77)
  (display "x: ")(write x)(newline)

  (display "x: ")(write (environment-ref (calling-environment) 'x))(newline)

  (environment-update! (calling-environment) 'y 88)
  (display "calling-environment: ")(write (environment-defined-symbols (calling-environment)))(newline)
  (newline)


)
(display "y: ")(write y)(newline)

;; (quit)

;; (display "auto_scheme: ")(write (environment-defined-symbols (environment-ref (current-environment) (string->symbol "(auto scheme)"))))(newline)
;; (display "current-environment: ")(write (environment-defined-symbols (current-environment)))(newline)




(define-library (mylib)
  (import (only (auto scheme) 
		let begin define 

		quote
		write
		newline
		)
	  ;; (auto scheme environment)
	  )
  (export (rename a aa) b c x y z zz)
  (begin 

    (define a 1) 
    (define b 2) 
    (define c 3) 
    (define d 4) 
    (define e 5) 
    (define f 6) 
    (define x 7) 
    (define y 8) 
    (define z 9)
    (define zz 99)
;; (write (current-environment))(newline)
    )
  )


(display "(mylib): ")(write (environment-ref (current-environment) (string->symbol "(mylib)")))
(newline)(newline)
(display "new env: ")(write (apply make-environment '((m . 66)(n . 77)(o . 88) (p . 99))))
(newline)

(import (only (mylib) aa b c)
	(prefix (except (only (mylib) x y z) y) pre-)


 )




(write (environment-defined-symbols (current-environment)))
(newline)(newline)

(display "aa: ")(write aa)(newline)
(display "b: ")(write b)(newline)
(display "c: ")(write c)(newline)

(display "pre-x: ")(write pre-x)(newline)
;; (display "y: ")(write y)(newline)
(display "pre-z: ")(write pre-z)(newline)

(import (auto scheme eval)
	(auto scheme library)
	)

(write (environment (only (mylib) aa b c)
		    (prefix (except (only (mylib) x y z) y) pre-)
		    ))


(newline)
(display "this-->")(newline)
(write (environment (auto scheme environment)))(newline)
(write (environment (auto scheme library)))(newline)
(newline)