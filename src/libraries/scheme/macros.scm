

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

  (let ()

    (define import-bindings
      (lambda (set)
	(cond ((not (pair? set)) (error "improper import-set:" set))
	      ((equal? (car set) 'only) (apply import-only (cdr set)))
	      ((equal? (car set) 'except) (apply import-except (cdr set)))
	      ;; ((equal? (car set) 'prefix) (apply import-prefix (cadr set)))
	      ;; ((equal? (car set) 'rename) (apply import-rename (cdr set)))
	      (else `(let-ref (curlet) (symbol ,(object->string set))))
	      )

	))

    (define import-only
      (lambda (set . identifiers)
	`(apply inlet ,(cons 'list (map (lambda (identifier)
					  `(cons ',identifier (let-ref ,(import-bindings set) ',identifier))
					  )
					identifiers)))
	))

    (define import-except
      (lambda (set . identifiers)
	`(apply cutlet (cons (apply inlet (let->list ,(import-bindings set))) ,identifiers))
	))

    ;; (define import-prefix
    ;;   (lambda (set identifier)


    ;;     ;; `(apply inlet ,(cons 'list (map (lambda (identifier)
    ;;     ;; 				      `(cons ',identifier (let-ref ,(import-bindings set) ',identifier))
    ;;     ;; 				      )
    ;;     ;; 				    identifiers)))

    ;;     ))

    ;; (define import-rename
    ;;   (lambda (set . modifiers)
    ;; 	(display "import-renaming...")(newline)
    ;; 	(display "set: ")(write set)(newline)
    ;; 	(display "modifiers: ")(write modifiers)(newline)

    ;; 	(write

	 
    ;; 	 `(let ((env-only ,(apply import-only (cons set (map car modifiers))))
    ;; 		(env-except ,(apply import-except (cons set (map car modifiers))))
    ;; 		)

    ;; 	    (apply varlet (cons env-except ,(cons 'list (map (lambda (modifier)
    ;; 							       `(cons ',(cadr modifier) (let-ref ,(import-bindings set) ',(car modifier)))
    ;; 							       )
    ;; 							     modifiers))))


	    
    ;; 	    env
    ;; 	    )

    ;; 	 )(newline)
    ;; 	  (import-bindings set)

    ;; 	  ))




    (cons 'begin (map (lambda (set)
			`(apply varlet (cons (curlet) (let->list ,(import-bindings set))))
			)
		      sets))

    )
  )


