;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define object->string
  (lambda (object)
    (let ((string-port (open-output-string)))
      (write object string-port)
      (let ((output-string (get-output-string string-port)))
	(close-output-port string-port)
	output-string))
    ))


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

    (let ((environment-declaration (cons 'environment (map cadr import-declarations)))
	  )

      (_quasiquote (environment-update! (current-environment) ',(string->symbol (object->string name)) 
					(eval
					 '(begin
					    (_unquote-splicing begin-declarations)	   
					    (environment-rename (environment-only (current-environment) 

										  (_unquote-splicing (map (lambda (sym) (_quasiquote (quote ,sym))) export-only)))

								(_unquote-splicing (map (lambda (sym) (_quasiquote (quote ,sym))) export-rename)))
					    )
					    (_unquote environment-declaration)
					    
					    )
					 )))))
   




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
	      (_quasiquote (environment-ref (current-environment) ',(string->symbol (object->string set))))))

	   (import-only
	    (lambda (set . identifiers)
	      (_quasiquote (environment-only ,(import-bindings set) (_unquote-splicing (map (lambda (sym) (_quasiquote (quote ,sym))) identifiers) )))))

	   (import-except
	    (lambda (set . identifiers)
	      (_quasiquote (environment-except ,(import-bindings set) (_unquote-splicing (map (lambda (sym) (_quasiquote (quote ,sym))) identifiers))))))

	   (import-prefix
	    (lambda (set identifier)
	      (_quasiquote (environment-prefix ,(import-bindings set) ',identifier))))

	   (import-rename
	    (lambda (set . rename-list)
	      (_quasiquote (environment-rename ,(import-bindings set) (_unquote-splicing (map (lambda (rename) (_quasiquote (quote ,(cons (car rename) (cadr rename))))) rename-list))))))


	   )

    (_quasiquote (environment-import! (current-environment) (_unquote-splicing (map (lambda (set)
										      (import-bindings set))
										    sets) )))
    )
  )


(define-macro (environment . sets)
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
	      (_quasiquote (environment-ref (current-environment) ',(string->symbol (object->string set))))))

	   (import-only
	    (lambda (set . identifiers)
	      (_quasiquote (environment-only ,(import-bindings set) (_unquote-splicing (map (lambda (sym) (_quasiquote (quote ,sym))) identifiers) )))))

	   (import-except
	    (lambda (set . identifiers)
	      (_quasiquote (environment-except ,(import-bindings set) (_unquote-splicing (map (lambda (sym) (_quasiquote (quote ,sym))) identifiers))))))

	   (import-prefix
	    (lambda (set identifier)
	      (_quasiquote (environment-prefix ,(import-bindings set) ',identifier))))

	   (import-rename
	    (lambda (set . rename-list)
	      (_quasiquote (environment-rename ,(import-bindings set) (_unquote-splicing (map (lambda (rename) (_quasiquote (quote ,(cons (car rename) (cadr rename))))) rename-list))))))


	   )

    (_quasiquote 
     (let ((env (make-environment)))

       (environment-import! env (_unquote-splicing (map (lambda (set)
							  (import-bindings set))
							sets) ))

       env))
    )
  )
