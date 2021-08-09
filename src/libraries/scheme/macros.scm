(define-macro (define-library name . declarations)
  (let ((export-declarations '())
	(import-declarations '())
	(begin-declarations '())

	(export-only '())
	(export-rename '())
	)

    (for-each (lambda (declaration)
		(cond ((eq? (car declaration) 'export) (set! export-declarations (cons declaration export-declarations)))
		      ((eq? (car declaration) 'import) (set! import-declarations (cons declaration import-declarations)))
		      ((eq? (car declaration) 'begin) (set! begin-declarations (cons declaration begin-declarations)))
		      (else (error 'define-library "unknown declaration type:" (car declaration)))))
	      (reverse declarations))

    (for-each (lambda (declaration)
		(for-each (lambda (spec)
			    (cond ((symbol? spec) (set! export-only (cons spec export-only)))
				  ((and (list? spec) (= (length spec) 3) (equal? (car spec) 'rename)) 
				   (set! export-only (cons (cadr spec) export-only))
				   (set! export-rename (cons (cons (cadr spec) (caddr spec)) export-rename)))
				  (else (error 'define-library "unknown export spec:" spec)))
			    )
			  (cdr declaration))
		)
	      export-declarations)

    `(environment-update! (current-environment) ',(symbol (object->string name)) 
			  (let ()
			    ,@import-declarations
			    ,@begin-declarations	   
			    (environment-rename (environment-only (current-environment) ',export-only) ',export-rename)
			    )
			  )
    ))



(define-macro (import . sets)
  (letrec ((import-bindings
	    (lambda (set)
	      (cond ((not (pair? set)) (error "improper import-set:" set))
		    ((equal? (car set) 'only) (apply import-only (cdr set)))
		    ((equal? (car set) 'except) (apply import-except (cdr set)))
		    ((equal? (car set) 'prefix) (apply import-prefix (cdr set)))
		    ((equal? (car set) 'rename) (apply import-rename (cdr set)))
		    (else (import-library set))
		    )
	      ))

	   (import-library
	    (lambda (set)
	      `((current-environment) (symbol ,(object->string set)))))

	   (import-only
	    (lambda (set . identifiers)
	      `(environment-only ,(import-bindings (list (car set))) '(,@identifiers))))

	   (import-except
	    (lambda (set . identifiers)
	      `(environment-except ,(import-bindings (list (car set))) '(,@identifiers))))

	   (import-prefix
	    (lambda (set identifier)
	      `(environment-prefix ,(import-bindings (list (car set))) ',identifier)))

	   (import-rename
	    (lambda (set . rename-list)
	      
	      `(environment-rename ,(import-bindings (list (car set))) '(,@(map (lambda (rename) (cons (car rename) (cadr rename))) rename-list)))))
	   )

    `(environment-import! (current-environment) ,(cons 'list (map (lambda (set)
								    (import-bindings set))
								  sets)))
    )
  )


