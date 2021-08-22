(define make-environment inlet)
(define current-environment curlet)
(define environment-ref let-ref)
(define environment-remove! cutlet)

(define environment-defined
  (lambda (environment)
    (let get-defined((remainder (let->list environment))
		     )
      (cond ((null? remainder) '())
	    ((equal? (cdar remainder) #<undefined>) (get-defined (cdr remainder)))
	    (else (cons (caar remainder) (get-defined (cdr remainder))))
	    ))))

(define environment-defined? 
  (lambda (environment symbol)
    (not (equal? (environment-ref environment symbol) #<undefined>))))

(define environment-update! 
  (lambda (environment symbol val)
    (varlet environment symbol val)))


(define environment-import! 
  (let ((_for-each for-each))
    (lambda (target environment-list)
      (_for-each (lambda (e)
		   (_for-each (lambda (binding)
				(environment-update! target (car binding) (cdr binding))
				)
			      (let->list e))
		   )
		 environment-list))))


(define environment-only
  (lambda (environment symbol-list)
    (apply make-environment (map (lambda (symbol)
				   (cons symbol (environment symbol))
				   )
				 symbol-list))))


(define environment-except
  (let ((_for-each for-each))
    (lambda (environment symbol-list)
      (let ((target (make-environment))
	    (bindings (let->list environment))
	    )
	(_for-each (lambda (binding)
		     (if (not (member (car binding) symbol-list))
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
    (lambda (environment association-list)
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

