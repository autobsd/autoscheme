;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define environment-import-sets!
  (lambda (target source . sets)

    (letrec ((process-import-set
	      (lambda (set)

		(if (not (pair? set)) (error "improper import-set:" set)
		    (cond ((equal? (car set) 'only) (apply environment-only (cons (process-import-set (cadr set)) (cddr set))))
			  ((equal? (car set) 'except) (apply environment-except (cons (process-import-set (cadr set)) (cddr set))))
			  ((equal? (car set) 'prefix) (environment-prefix (process-import-set (cadr set)) (caddr set)))
			  ((equal? (car set) 'rename) (apply environment-rename (cons (process-import-set (cadr set)) (cddr set))))
			  (else (environment-ref source (string->symbol (object->string set))))
			  ))))
	     
	     )
      (for-each (lambda (set)

		  (environment-import! target (process-import-set set))
		  )
		sets)

      target
      )))




(define library-eval
  (lambda (declarations environment)

    (letrec ((export-declarations '())
	     (import-declarations '())
	     (begin-declarations '())

	     (export-only '())
	     (export-rename '())

	     (library-environment (make-environment))
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
		  (apply environment-import-sets! (cons library-environment (cons environment (cdr declaration))))
		  )
		import-declarations)


      (eval
       `(begin
	  ,@begin-declarations
	  )
       library-environment
       )

      (apply environment-rename (cons (apply environment-only (cons library-environment export-only)) export-rename))

      ))
  )

