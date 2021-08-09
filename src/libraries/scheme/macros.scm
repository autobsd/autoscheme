(define-macro (define-library name . declarations)
  (let ((export-declarations '())
	(import-declarations '())
	(begin-declarations '())
	(export-symbols '())
	)

    (map (lambda (declaration)
	   (cond ((eq? (car declaration) 'export) (set! export-declarations (cons declaration export-declarations)))
		 ((eq? (car declaration) 'import) (set! import-declarations (cons declaration import-declarations)))
		 ((eq? (car declaration) 'begin) (set! begin-declarations (cons declaration begin-declarations)))
		 (else (error 'define-library "unknown declaration type:" (car declaration)))))
	 (reverse declarations))

    (map (lambda (declaration)
	   (set! export-symbols (append export-symbols (cdr declaration))))
	 export-declarations)

    (let* ((exported-env-expression (cons 'inlet (map (lambda (sym)
							``(,',sym . ,,sym))
						      export-symbols)))
	   (block `(let ()
		     ,@import-declarations
		     ,@begin-declarations
		     ,exported-env-expression
		     )))

      `(varlet (curlet) `(,(symbol (object->string ',name)) . ,,block)))
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


