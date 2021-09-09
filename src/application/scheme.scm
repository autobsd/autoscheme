;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(display "inside AutoScheme application...\n")

(define object->string
  (lambda (object)
    (let ((string-port (open-output-string)))
      (write object string-port)
      (let ((output-string (get-output-string string-port)))
	(close-output-port string-port)
	output-string))
    ))

(include "list.scm")
(include "environment.scm")

(include "macros.scm")


;; (define x 1)
(let ((y 2))
  ;; (define bindings '((a . 10) (b . 20) (c . 30)))
  ;; (define env (apply make-environment bindings))
  ;; (write env)(newline)
  ;; (environment-update! env 'a 9)
  ;; (environment-update! env 'd 99)
  ;; (write (environment-delete! env 'b))(newline)

  ;; (write (environment-only env 'a))(newline)
  ;; (write (environment-except env 'a))(newline)
  ;; (write (environment-rename env '(d . e)))(newline)
  ;; (write (environment-prefix env 'pre-))(newline)

  ;; ;; (write (environment-symbols (current-environment)))(newline)



  ;; (write '(env))(newline)
  ;; (write (string->symbol "(env)"))(newline)

  ;; (environment-update! (current-environment) (string->symbol "(env)") env)
  ;; ;; (write (macro-expand '(import (env))))(newline)
  ;; (import (env))

  ;; (write (car (current-environment)))(newline)



  (write (macro-expand 
	  '(define-library (mylib)
	     (export myvar)
	     (begin
	       (define myvar 777)
	       ))
	  )) 
  (newline)

  (write (macro-expand 
	  '(import (mylib))
	  )) 
  (newline)



  (define-library (mylib)
    (export myvar (rename myvar2 myvar3))
    (begin
      (define myvar 777)
      (define myvar2 888)
      ))


  
  (import (rename (mylib) (myvar3 myvar4)))


  (display "myvar: ")(write myvar)(newline)
  (display "myvar4: ")(write myvar4)(newline)

  (quit)

  ;; (display (environment? env))(newline)
  ;; (display (defined? 'env))(newline)

  ;; (display (environment-assoc (current-environment) 'env))(newline)
  ;; (display (environment-assoc (current-environment) 'x))(newline)
  ;; (display (environment-assoc (current-environment) 'y))(newline)

  ;; (display "x: ")(write (environment-ref (current-environment) 'x))(newline)
  ;; (display "y: ")(write (environment-ref (current-environment) 'y))(newline)
  ;; (display "list?: ")(write (list? '(a . ())))(newline)
  
  ;; (display (defined? 'b env))(newline)
  ;; (display (environment-assoc env 'b))(newline)

  ;;  (display "a: ")(write a)(newline)
  )


