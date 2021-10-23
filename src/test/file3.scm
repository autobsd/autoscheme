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
	(auto scheme path)
	)

(display "testing:\n")

;; (write (current-directory)) (newline)
;; (write (command-line))(newline)
;; (write (get-environment-variables))(newline)
;; (write (get-environment-variable "HOME"))(newline)

;; (newline)
;; (display "+: ")(write +)(newline)
;; (display "macro?: ")(write (macro? +))(newline)
;; (display "closure?: ")(write (closure? +))(newline)
;; (display "procedure?: ")(write (procedure? +))(newline)


;; (define-macro (mymac x)
;;   `(+ ,x 5)
;;   )
;; (newline)
;; (display "mymac: ")(write mymac)(newline)
;; (display "macro?: ")(write (macro? mymac))(newline)
;; (display "closure?: ")(write (closure? mymac))(newline)
;; (display "procedure?: ")(write (procedure? mymac))(newline)




;; (define (add2 x)
;;   (+ x 2))

;; (newline)
;; (display "add2: ")(write add2)(newline)
;; (display "macro?: ")(write (macro? add2))(newline)
;; (display "closure?: ")(write (closure? add2))(newline)
;; (display "procedure?: ")(write (procedure? add2))(newline)

;; (newline)
;; (display "environment-only: ")(write environment-only)(newline)
;; (display "macro?: ")(write (macro? environment-only))(newline)
;; (display "closure?: ")(write (closure? environment-only))(newline)
;; (display "procedure?: ")(write (procedure? environment-only))(newline)


;; (newline)
;; (define path "/some/path/to/file.scm")
;; (display "path: ")(write path)(newline)
;; (display "path-directory: ")(write (path-directory path))(newline)
;; (gc-verbose #t)
;; (collect-garbage)

;; (write (last-pair '(a b c)))(newline)

;; (write (get-closure-code (lambda () (display "message"))))(newline)

;; (define expression (delay (display "evaluating expression now")(newline)))

;; (display "expression defined")(newline)

;; (force expression)
;; (force expression)

;; (display (environment-defined-symbols (environment-ref (current-environment) (string->symbol "(auto scheme base)"))))(newline)

;; (newline)
;; (define my-param (make-parameter 5 (lambda (x) (+ x 1))))
;; (display "(my-param): ")(write (my-param))(newline)
;; (display "(my-param): ")(write (my-param))(newline)
;; (display "(my-param 3): ")(write (my-param 3))(newline)
;; (display "(my-param): ")(write (my-param))(newline)
;; (display "(my-param): ")(write (my-param))(newline)
;; (display "(my-param 7): ")(write (my-param 7))(newline)
;; (display "(my-param): ")(write (my-param))(newline)

;; (parameterize ((my-param 2))
;; 	      (display "(my-param): ")(write (my-param))(newline)
;; 	      )

;; (display "(my-param): ")(write (my-param))(newline)


;; (include "file2.scm")


;; (display "x: ")(write x)(newline)


(define x (quote (a b . c) 5 5))
(display "x: ")(write x)(newline)
(display "(list? x): ")(write (list? x))(newline)