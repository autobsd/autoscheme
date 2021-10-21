;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (auto scheme write)
	(auto scheme base) 
	(auto scheme environment)
	;; (except (auto scheme base) lambda)
	(auto scheme directory)
	(scheme process-context)
	(auto scheme macro)
	(auto scheme list)
	(auto scheme lazy)
	(auto scheme memory)
	(auto scheme closure)
	)

(display "testing:\n")

(write (current-directory)) (newline)
(write (command-line))(newline)
(write (get-environment-variables))(newline)
(write (get-environment-variable "HOME"))(newline)


(define-macro (mymac x)
  `(+ ,x 5)
  )
(write mymac) (newline)
(write (mymac 3)) (newline)
(write (macro? mymac))(newline)

;; (write (macro-expand '(mymac 3)))(newline)

(define (add2 x)
  (+ x 2))

(write (add2 33))(newline)

(write add2)(newline)
(write (closure? add2))(newline)
(write (procedure? add2))(newline)

(define add3 (lambda (x)
  (+ x 3)))

(write add3)(newline)
(write (closure? add3))(newline)
(write (procedure? add3))(newline)

(write environment-only)(newline)
(write (closure? environment-only))(newline)
(write (procedure? environment-only))(newline)

(define add7 
  (macro (x)
    (+ x 7)))

(write (add7 3))(newline)

(gc-verbose #t)
(collect-garbage)

(write (last-pair '(a b c)))(newline)

(write (get-closure-code (lambda () (display "message"))))(newline)

(define expression (delay (display "evaluating expression now")(newline)))

(display "expression defined")(newline)

(force expression)
(force expression)

(display (environment-defined-symbols (environment-ref (current-environment) (string->symbol "(auto scheme base)"))))(newline)

(newline)
(define my-param (make-parameter 5 (lambda (x) (+ x 1))))
(display "(my-param): ")(write (my-param))(newline)
(display "(my-param): ")(write (my-param))(newline)
(display "(my-param 3): ")(write (my-param 3))(newline)
(display "(my-param): ")(write (my-param))(newline)
(display "(my-param): ")(write (my-param))(newline)
(display "(my-param 7): ")(write (my-param 7))(newline)
(display "(my-param): ")(write (my-param))(newline)

(parameterize ((my-param 2))
	      (display "(my-param): ")(write (my-param))(newline)
	      )

(display "(my-param): ")(write (my-param))(newline)

;; (write
;; (include_ "file1.scm" 
;; 	  "file2.scm")
;; )(newline)