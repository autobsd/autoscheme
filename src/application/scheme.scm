;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(display "inside AutoScheme application...\n")

(define env (make-environment (cons 'begin begin)
			      (cons 'display display)
			      (cons 'newline newline)
			      (cons 'write write)
			      (cons 'var "val")
			      
))

(eval '(begin (display "hello world, var:")(write var) (newline)) env)




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
  ;; (environment-delete! (current-environment) 'myvar4)
  
  (newline)
  (write (macro-expand '(environment (only (mylib) myvar))))(newline)
  (define env (environment (only (mylib) myvar)))
  (write env)(newline)

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


