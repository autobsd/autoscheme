;; (scheme base)
(define-library (auto scheme base)
  (import (only (s7) 
          + - * / < <= = > >= append apply assoc boolean? car caar cadr call-with-current-continuation call/cc cdar cddr cdr char->integer char=? char? close-input-port close-output-port cons eof-object? equal? for-each integer? length map member min negative? newline not null? number? number->string pair? positive? read-string reverse string->symbol string-append string-length string=? string? substring symbol->string symbol? values vector-ref zero?))

  (import (only (s7) 
		_begin
		_list
		_if
		_cond
		_else
		_and
		))

  (export + - * / < <= = > >= append apply assoc boolean? car caar cadr call-with-current-continuation call/cc cdar cddr cdr char->integer char=? char? close-input-port close-output-port cons eof-object? equal? for-each integer? length map member min negative? newline not null? number? number->string pair? positive? read-string reverse string->symbol string-append string-length string=? string? substring symbol->string symbol? values vector-ref zero?)

  (export (rename _begin begin)
	  (rename _list list) 
	  (rename _if if)
	  (rename _cond cond)
	  (rename _else else)
	  (rename _and and)
	  
	  )

  


  (export vector-for-each)


  (begin

    (define read-string 
      (let ((s7_read-string read-string))
	(lambda args
	  (let ((k (if (null? args) #f (car args)))
		(rest (if (pair? args) (cdr args) '()))
		)
	    (if (not k)
		(let ((s (apply s7_read-string (cons 64 rest))))
		  (if (eof-object? s) 
		      ""
		      (string-append s (apply read-string (cons #f rest)))))
		(apply s7_read-string args))
	    ))))


    (define vector-for-each
      (lambda (proc . vectors)
	(let* ((min-length (apply min (map length vectors)))
	       )
	  (do ((i 0 (+ i 1)))
	      ((= i min-length) i)
	    (apply proc (map (lambda (v)
			       (vector-ref v i))
			     vectors))
	    ))
	))

    )
  )



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






