;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(display "inside AutoScheme application...\n")


(foreign-declaration (include-string "declarations.h"))
(foreign-definition (include-string "definitions.c"))

(foreign-initialization "scheme_register_foreign_func( \"make-environment\", mk_environment );\n")

(define environment-binding 
  (lambda (environment symbol)
    (let lookup((remainder environment)
		)
      (cond ((null? remainder) #f)
	    ((assoc symbol (car remainder)))
	    (else (lookup (cdr remainder)))))))

(define environment-bound?
  (lambda (environment symbol)
    (defined? symbol environment)))

(define environment-ref
  (lambda (environment symbol)
    (cdr (environment-binding environment symbol))))




;; (define environment-delete!
;;   (lambda (environment symbol)

;;     (let delete!((remainder environment)
;; 		 )
;;       (cond ((null? remainder) #f)
;; 	    ((assoc symbol (car remainder)))
;; 	    (else (lookup (cdr remainder)))))))







(define x 1)
(let ((y 2))
  (define bindings '((a . 10) (b . 20) (c . 30)))
  (define env (apply make-environment bindings))
  (write env)(newline)
  (display (environment? env))(newline)
  (display (defined? 'env))(newline)

  (display (environment-binding (current-environment) 'env))(newline)
  (display (environment-binding (current-environment) 'x))(newline)
  (display (environment-binding (current-environment) 'y))(newline)

  (display "x: ")(write (environment-ref (current-environment) 'x))(newline)
  (display "y: ")(write (environment-ref (current-environment) 'y))(newline)
  (display "list?: ")(write (list? '(a . ())))(newline)
  
  (display (defined? 'b env))(newline)
  (display (environment-binding env 'b))(newline)
  (display (environment-bound? env 'b))(newline)
;;  (display "a: ")(write a)(newline)
)


