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
  (let ((name-symbol (string->symbol (object->string name)))
	(quoted-declarations (map (lambda (declaration)
				    (_quasiquote ',declaration)
				    )
				  declarations))
	)
  (quasiquote (environment-define! (current-environment) ',name-symbol (make-library (_unquote-splicing quoted-declarations))))
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
