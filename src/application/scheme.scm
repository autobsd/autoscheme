;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (auto scheme)
	)

(display "inside AutoScheme application...\n")



(define-library (mylib)
  (import (only (auto scheme) 
		let begin define 
		environment-rename 
		environment-only
		current-environment
		quote
		write
		newline
		))
  (export (rename a aa) b c x y z)
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
(write (current-environment))(newline)
    )
  )


(write (environment-ref (current-environment) (string->symbol "(mylib)")))
(newline)(newline)

(import (only (mylib) aa b c)
	(prefix (except (only (mylib) x y z) y) pre-)
	)


(display "aa: ")(write aa)(newline)
(display "b: ")(write b)(newline)
(display "c: ")(write c)(newline)

(display "pre-x: ")(write pre-x)(newline)
;; (display "y: ")(write y)(newline)
(display "pre-z: ")(write pre-z)(newline)



(write (environment (only (mylib) aa b c)
		    (prefix (except (only (mylib) x y z) y) pre-)
		    ))
(newline)

