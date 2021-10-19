;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (auto scheme write)
	(auto scheme base) 
	;; (except (auto scheme base) lambda)
	(auto scheme directory)
	(scheme process-context)
	(auto scheme macro)
	(auto scheme list)
	)

(display "testing:\n")

(write (current-directory)) (newline)
(write (command-line))(newline)
(write (get-environment-variables))(newline)
(write (get-environment-variable "HOME"))(newline)


(define-macro (mymac x)
  `(+ ,x 5)
  )

(write (mymac 3)) (newline)
(write (macro? mymac))(newline)

;; (write (macro-expand '(mymac 3)))(newline)

(define (add2 x)
  (+ x 2))

(write (add2 33))(newline)

(define add7 
  (macro (x)
    (+ x 7)))

(write (add7 3))(newline)


(write (last-pair '(a b c)))(newline)

(write (get-closure-code (lambda () (display "message"))))(newline)

(define expression (delay (display "evaluating expression now")(newline)))

(display "expression defined")(newline)

(force expression)
(force expression)