;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(display "inside AutoScheme application...\n")

(include "list.scm")
(include "environment.scm")







(define-macro (add1 x)

  (quasiquote (+ ,x 1))
  )

(write (macro? add1))(newline)
(write (macro-expand '(add1 5)))(newline)

(display "(add1 5): ")(write (add1 5))(newline)

;; (write (map car (car (current-environment))))(newline)


(define x 1)
(let ((y 2))
  (define bindings '((a . 10) (b . 20) (c . 30)))
  (define env (apply make-environment bindings))
  (write env)(newline)
  (environment-update! env 'a 9)
  (environment-update! env 'd 99)
  (write (environment-delete! env 'b))(newline)

  (write (environment-only env 'a))(newline)
  (write (environment-except env 'a))(newline)
  (write (environment-rename env '(d . e)))(newline)
  (write (environment-prefix env 'pre-))(newline)

  ;; (write (environment-symbols (current-environment)))(newline)



  (display (environment? env))(newline)
  (display (defined? 'env))(newline)

  (display (environment-assoc (current-environment) 'env))(newline)
  (display (environment-assoc (current-environment) 'x))(newline)
  (display (environment-assoc (current-environment) 'y))(newline)

  (display "x: ")(write (environment-ref (current-environment) 'x))(newline)
  (display "y: ")(write (environment-ref (current-environment) 'y))(newline)
  (display "list?: ")(write (list? '(a . ())))(newline)
  
  (display (defined? 'b env))(newline)
  (display (environment-assoc env 'b))(newline)

;;  (display "a: ")(write a)(newline)
)


