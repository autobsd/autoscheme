;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define make-library
  (lambda declarations

    (letrec ((export-declarations '())
	     (import-declarations '())
	     (begin-declarations '())

	     (export-only '())
	     (export-rename '())

	     (library-environment (make-environment))

	     (process-import-set
	      (lambda (set)
		(if (not (pair? set)) (error "improper import-set:" set)
		    (cond ((equal? (car set) 'only) (apply environment-only (cons (process-import-set (cadr set)) (cddr set))))
			  ((equal? (car set) 'except) (apply environment-except (cons (process-import-set (cadr set)) (cddr set))))
			  ((equal? (car set) 'prefix) (environment-prefix (process-import-set (cadr set)) (caddr set)))
			  ((equal? (car set) 'rename) (apply environment-except (cons (process-import-set (cadr set)) (cddr set))))
			  (else (environment-ref (current-environment) (string->symbol (object->string set))))
			  ))))

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

      (for-each (lambda (declaration)
		  (for-each (lambda (set)
			      (environment-import! library-environment (process-import-set set))
			      )
			    (cdr declaration))
		  )
		import-declarations)
      


      (eval
       (_quasiquote (begin
		      (_unquote-splicing begin-declarations)	   
		      (environment-rename (environment-only (current-environment) 

							    (_unquote-splicing (map (lambda (sym) (_quasiquote (quote ,sym))) export-only)))

					  (_unquote-splicing (map (lambda (sym) (_quasiquote (quote ,sym))) export-rename)))
		      ))

       library-environment

       
       )
      )))

