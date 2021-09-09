;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

;; (auto scheme environment)
(foreign-declaration (include-string "declarations.h"))
(foreign-definition (include-string "definitions.c"))
(foreign-initialization "scheme_register_foreign_func( \"make-environment\", mk_environment );\n")

(define environment-symbols
  (lambda (environment)
    (map car (apply append environment))))


(define environment-assoc 
  (lambda (environment symbol)
    (let lookup((remainder environment)
		)
      (cond ((null? remainder) #f)
	    ((assoc symbol (car remainder)))
	    (else (lookup (cdr remainder)))))))


(define environment-ref
  (lambda (environment symbol)
    (cdr (environment-assoc environment symbol))))


(define environment-delete!
  (lambda (environment symbol)
    (let ((binding-lists (map (lambda (alist)
				(if (assoc symbol alist)
				    (alist-delete alist symbol)
				    alist)
				)
			      environment)))

      (set-car! environment (car binding-lists))
      (set-cdr! environment (cdr binding-lists))
      environment)))


(define environment-update!
  (lambda (environment symbol value)
    (let ((association (environment-assoc environment symbol))
	  )
      (if association (set-cdr! association value)
	  (set-car! environment (cons (cons symbol value) (car environment))))
      environment)))


(define environment-import!
  (lambda (target . environments)
    (for-each (lambda (env)
		;; (set-car! target (append (reverse (apply append env))(car target)))
		(for-each (lambda (association)
			    (environment-update! target (car association)(cdr association))
			    )
			  (append (reverse (apply append env))(car target)))
 		)
	      environments)
    target))


(define environment-only
  (lambda (environment . symbols)
    (apply make-environment (map (lambda (sym)
				   (let ((association (environment-assoc environment sym)))
				     (cons (car association)(cdr association))))
				 symbols))))


(define environment-except
  (lambda (environment . symbols)
    (let ((target (environment-import! (make-environment) environment)))
      (for-each (lambda (sym)
      		  (environment-delete! target sym)
		  )
		symbols)
      target)))


(define environment-prefix
  (lambda (environment symbol)
    (let ((target (environment-import! (make-environment) environment)))
      (for-each (lambda (association)
		  (set-car! association (string->symbol (string-append (symbol->string symbol) (symbol->string (car association))))))
		(car target))
      target)))


(define environment-rename
  (lambda (environment . association-list)
    (let ((target (environment-import! (make-environment) environment)))
      (for-each (lambda (association)

		  (set-car! (environment-assoc target (car association)) (cdr association)))
		association-list)
      target)))
  
    