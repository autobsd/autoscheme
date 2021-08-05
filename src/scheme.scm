;; (set! (*s7* 'print-length) 1024)

(define s7_object->string object->string)
(define object->string
  (lambda (obj)
    (let ((str (s7_object->string obj)))
      (if (string? obj) 
	  (with-output-to-string (lambda ()
				   (do ((i 0 (+ i 1)))
				       ((= i (string-length str)) )      
				     (cond ((member (str i) `(,("\n" 0) #\newline))(display "\\n"))
					   (else (display (str i)))))))
	  str
	  ))))


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

  (cons 'begin (map (lambda (set)
		      `(apply varlet (cons (curlet) (let->list (let-ref (curlet) (symbol ,(object->string set))))))
		      )
		    sets))
  )


(define-library (s7)

  (export + - * / < <= = > >=)                           
  (export caaaar)
  (export eval)
  (export acos)
  (export load)
  (export read)
  (export display write)
  (export caaaar caaar caaddr cadaar cadar cadddr cdaaar cdaar cdaddr cddaar cddar cddddr
	caaadr caadar caadr cadadr caddar caddr cdaadr cdadar cdadr cddadr cdddar cdddr)
  )


(cutlet (rootlet) '+ '- '* '/ '< '<= '= '> '>=)
(cutlet (rootlet) 'caaaar)
(cutlet (rootlet) 'eval)
(cutlet (rootlet) 'acos)
(cutlet (rootlet) 'read)
(cutlet (rootlet) 'load)
(cutlet (rootlet) 'display 'write)
(cutlet (rootlet) 'caaaar 'caaar 'caaddr 'cadaar 'cadar 'cadddr 'cdaaar 'cdaar 'cdaddr 'cddaar 'cddar 'cddddr
	'caaadr 'caadar 'caadr 'cadadr 'caddar 'caddr 'cdaadr 'cdadar 'cdadr 'cddadr 'cdddar 'cdddr)

;; (varlet *source* 'directory
;; 	    (lambda ()

;; 	      (let ((pos -1)
;; 		    )
;; 		(do ((len (length (let-ref *source* 'path)))
;; 		     (i 0 (+ i 1)))
;; 		    ((= i len))
;; 		  (if (char=? ((let-ref *source* 'path) i) #\/)
;; 		      (set! pos i)))
;; 		(if (positive? pos)
;; 		    (let ((directory-name (substring (let-ref *source* 'path) 0 pos)))
;; 		      directory-name)))))

;; (varlet *source* 'absolute?
;; 	    (lambda (path)
;; 	      (char=? (path 0) #\/)))


;; (varlet *source* 'make-absolute
;; 	    (lambda (path)
;; 	      (if ((let-ref *source* 'absolute?) path) path
;; 		  (string-append ((let-ref *source* 'directory)) "/" path))))

;; (varlet *source* 'previous #<undefined> 'value #<undefined> )

;; (define-macro (include . filenames)
;;   (let ()
;; 	(cons 'begin (map (lambda (filename)
;; 			    `(begin (let-set! *source* 'previous (let-ref *source* 'path))
;; 				    (let-set! *source* 'path ,((let-ref *source* 'make-absolute) filename))
;; 				    (let-set! *source* 'value (load ,(string-append ((let-ref *source* 'directory)) "/" filename)))
;; 				    (let-set! *source* 'path (let-ref *source* 'pevious))
;; 				    (let-ref *source* 'value)))

;; 			  filenames)
;; 	      )))



;; (define expand
;;   (lambda (expression env)
;; 	(cond ((not (pair? expression)) expression)
;; 	      (else (varlet env 'expression expression)
;; 		    (with-let env (let* ((first (car expression))
;; 					 (expanded-first (cond ((not (pair? first)) first)
;; 							       ((and (equal? (car first) 'define-macro)) (eval first) #<unspecified>)
;; 							       ((and (symbol? (car first))(macro? (symbol->value (car first)))) (expand (apply macroexpand (list first)) (sublet (curlet))))
;; 							       (else (cons (expand (car first) (sublet (curlet)))
;; 									   (expand (cdr first) (sublet (curlet)))))))
;; 					 )
;; 				    (cons expanded-first
;; 					  (expand (cdr expression) (sublet (curlet)))
;; 					  ))))))
;;   )


















