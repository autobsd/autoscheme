;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (only (auto scheme) 
	      display
	      newline
	      write
	      quit
	      environment-defined-symbols
	      current-environment
	      let
	      define
	      lambda
	      ;; calling-environment
	      length
	      )
	(auto scheme base)

	)
(display "inside regression test...\n")
;; (define test #t)

(newline)
((lambda (parent-environment)

   (import (except (auto scheme base) +)
	   (auto scheme list)
	   )
   (define alist '((a . 1)(b . 2)(c . 3)(d . 4)(e . 5)(f . 6)))
   (display "alist: ")(write alist)(newline)
   (define new-alist (alist-delete! alist 'b))
   (display "new-alist: ")(write new-alist)(newline)
   (display "eq?: ")(write (eq? alist new-alist))(newline)
   ;; (quit)
   ) (current-environment))


(define x 7)
(display "x: ")(write x)(newline)
(newline)

(let ()
  (define x 77)
  (display "x: ")(write x)(newline)

  ;; (display "x: ")(write (environment-ref caller-env 'x))(newline)

  ;; (environment-update! caller-env 'y 88)
  ;; (display ";; calling-environment: ")(write (environment-defined-symbols caller-env))(newline)
  (newline)


)
;; (display "y: ")(write y)(newline)
(display "x: ")(write x)(newline)

;; ;; (quit)

;; ;; (display "auto_scheme: ")(write (environment-defined-symbols (environment-ref (current-environment) (string->symbol "(auto scheme)"))))(newline)
;; ;; (display "current-environment: ")(write (environment-defined-symbols (current-environment)))(newline)




(define-library (mylib)
  (import (only (auto scheme) 
		define 
		begin

		)
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

    )
  )

(import (only (auto scheme) 
	      apply
	      string->symbol
	      make-environment
	      environment-ref
	      ))
(display "(mylib): ")(write (environment-ref (current-environment) (string->symbol "(mylib)")))
(newline)(newline)
(display "new env: ")(write (apply make-environment '((m . 66)(n . 77)(o . 88) (p . 99))))
(newline)

(import (only (mylib) aa b c)
	(prefix (except (only (mylib) x y z) y) pre-)
 )




(display "aa: ")(write aa)(newline)
(display "b: ")(write b)(newline)
(display "c: ")(write c)(newline)

(display "pre-x: ")(write pre-x)(newline)
;; (display "y: ")(write y)(newline)
(display "pre-z: ")(write pre-z)(newline)

(import (scheme eval)

	)

(newline)(newline)
(write (environment (only (mylib) aa b c)
		    (prefix (except (only (mylib) x y z) y) pre-)

		    ))
		   

(newline)

;; ;; (import (s-markup xml))

;; ;; (display-xml '((html (body (h1 "headline")))))(newline)

;; ;; (import (rename (auto scheme write) (display my-display) (write my-write)))
;; (import (auto scheme base)
;; 	(auto scheme write))
;; ;; (import (except (auto scheme write) display))


;; (display "test message")(newline)

;; (include "file1.scm")
;; (newline)
;; (display (include-string "../../version.txt"))(newline)

;; (import (auto scheme process context))
;; (display (command-line))(newline)
;; (display (current-directory))(newline)

;; ;; (import (auto scheme base))
;; ;; (import (auto scheme write))

;; ;; (display "Regression Tests")(newline)(newline)


;; ;; (display "(+ 2 2): ")(write (+ 2 2))(newline)
;; ;; (display "(- 5 2): ")(write (- 5 2))(newline)
;; ;; (display "(* 3 4): ")(write (* 3 4))(newline)
;; ;; (display "(/ 12 3): ")(write (/ 12 3))(newline)

;; ;; (display "(< 12 3): ")(write (< 12 3))(newline)

;; ;; (display "(<= 12 3): ")(write (<= 12 3))(newline)
;; ;; (display "(<= 3 3): ")(write (<= 3 3))(newline)

;; ;; (display "(= 12 3): ")(write (= 12 3))(newline)
;; ;; (display "(= 3 3): ")(write (= 3 3))(newline)

;; ;; (display "(> 12 3): ")(write (> 12 3))(newline)

;; ;; (display "(>= 12 3): ")(write (>= 12 3))(newline)
;; ;; (display "(>= 3 3): ")(write (>= 3 3))(newline)



