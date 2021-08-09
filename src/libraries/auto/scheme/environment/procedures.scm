(define make-environment inlet)
(define current-environment curlet)
(define environment-ref let-ref)

(define environment-defined? 
  (lambda (environment symbol)
    (not (equal? (environment-ref environment symbol) #<undefined>))))

(define environment-update! 
  (lambda (environment symbol val)
    (varlet environment symbol val)))


(define environment-import! 
  (lambda (target environment-list)
    (for-each (lambda (e)
		(for-each (lambda (binding)
			    (environment-update! target (car binding) (cdr binding))
			    )
			  (let->list e))
		)
	      environment-list)))


(define environment-only
  (lambda (environment symbol-list)
    (apply make-environment (cons environment (map (lambda (symbol)
						     (cons symbol (environment symbol))
						     )
						   symbol-list)))))


(define environment-except
  (lambda (environment symbol-list)
    (let ((target (make-environment))
	  (bindings (let->list environment))
	  )
      (for-each (lambda (binding)
		  (if (not (member (car binding) symbol-list))
		      (environment-update! target (car binding) (cdr binding)))
		  )
		bindings)
      target)))




(define environment-prefix
  (lambda (environment symbol)
    (let ((target (make-environment))
	  (bindings (let->list environment))
	  (prefix (symbol->string symbol))
	  )
      (for-each (lambda (binding)
		  (environment-update! target (string->symbol (string-append prefix (symbol->string (car binding)))) (cdr binding))
		  )
		bindings)
      target)))



(define environment-rename
  (lambda (environment association-list)
    (let ((target (make-environment))
	  (bindings (let->list environment))
	  )
      (for-each (lambda (binding)
		  (let ((rename (assoc (car binding) association-list)))
			
		  (if (not rename)
		      (environment-update! target (car binding) (cdr binding))
		      (environment-update! target (cdr rename) (cdr binding))
		      )
		  ))
		bindings)
      target)))
    