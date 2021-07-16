'(begin

   (define-macro (define-library name . declarations)
     (let ((export-declarations '())
   	   (begin-declarations '())
   	   (export-symbols '())
   	   )

       (map (lambda (declaration)
   	      (cond ((eq? (car declaration) 'export) (set! export-declarations (cons declaration export-declarations)))
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
   			,@begin-declarations
   		        ,exported-env-expression
   			)))

   	 `(varlet (curlet) `(,(symbol (object->string ',name)) . ,,block)))
       ))


   (define-macro (import . sets)

     (cons 'begin (map (lambda (set)
   			 `(apply varlet (cons (curlet) (let->list (let-ref (curlet) (symbol ,(object->string set))))))
   			 )
   		       sets))
     )

   

   (define-library (scheme base)
     (export + - * / < <= = > >=)
     )

   (cutlet (rootlet) '+ '- '* '/ '< '<= '= '> '>=)


   (define-library (scheme read)
     (export read)
     )

   (cutlet (rootlet) 'read)


   (define-library (scheme write)
     (export display)
     )

   (cutlet (rootlet) 'display)


   )