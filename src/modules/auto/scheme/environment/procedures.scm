;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define make-environment inlet)
(define current-environment curlet)
(define environment-ref let-ref)
(define environment-remove! cutlet)

(define environment-defined
  (let ((cond cond))
    (lambda (environment)
      (let ((bindings (let get-bindings((env environment))
			(let ((env-bindings (let->list env)))
			  (cond ((equal? env (rootlet)) env-bindings)
				(else (append env-bindings (get-bindings (outlet env)))))))
		      ))

	(let get-defined((remainder bindings)
			 )
	  (cond ((null? remainder) '())
		((equal? (cdar remainder) #<undefined>) (get-defined (cdr remainder)))
		(else (cons (caar remainder) (get-defined (cdr remainder))))
		))))))



(define environment-defined? 
  (lambda (environment symbol)
    (not (equal? (environment-ref environment symbol) #<undefined>))))



(define environment-update! 
  (lambda (environment symbol val)
    (varlet environment symbol val)))



(define environment-import! 
  (let ((_for-each for-each))
    (lambda (target . environments)
      (_for-each (lambda (e)
		   (_for-each (lambda (binding)
				(environment-update! target (car binding) (cdr binding))
				)
			      (let->list e))
		   )
		 environments))))



(define environment-only
  (lambda (environment . symbols)
    (apply make-environment (map (lambda (symbol)
				   (cons symbol (environment symbol))
				   )
				 symbols))))



(define environment-except
  (let ((_for-each for-each))
    (lambda (environment . symbols)
      (let ((target (make-environment))
	    (bindings (let->list environment))
	    )
	(_for-each (lambda (binding)
		     (if (not (member (car binding) symbols))
			 (environment-update! target (car binding) (cdr binding)))
		     )
		   bindings)
	target))))



(define environment-prefix
  (let ((_for-each for-each))
    (lambda (environment symbol)
      (let ((target (make-environment))
	    (bindings (let->list environment))
	    (prefix (symbol->string symbol))
	    )
	(_for-each (lambda (binding)
		     (environment-update! target (symbol (string-append prefix (symbol->string (car binding)))) (cdr binding))
		     )
		   bindings)
	target))))



(define environment-rename
  (let ((_for-each for-each))
    (lambda (environment . association-list)
      (let ((target (make-environment))
	    (bindings (let->list environment))
	    )
	(_for-each (lambda (binding)
		     (let ((rename (assoc (car binding) association-list)))
		       
		       (if (not rename)
			   (environment-update! target (car binding) (cdr binding))
			   (environment-update! target (cdr rename) (cdr binding))
			   )
		       ))
		   bindings)
	target))))

