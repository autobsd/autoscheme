;; (scheme base)
(define-library (auto scheme base)
  (export + - * / < <= = > >=)
  (import (s7))
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






