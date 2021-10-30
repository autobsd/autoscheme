;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-define (include-string "definitions.c"))
(foreign-initialize (include-string "initialization.c"))

;; ((foreign-syntax LOC_DEF0 "define") display (foreign-operation LOC_DISPLAY))
;; ((foreign-syntax LOC_DEF0 "define") write (foreign-operation LOC_WRITE))
(define syntax-lambda (foreign-syntax LOC_SYNLAM "syntax-lambda"))



(define define-record-type
  (let ((environment-define! (foreign-function ff_environment_define_d))
	(gensym (foreign-operation LOC_GENSYM))
	(eval (foreign-operation LOC_PEVAL))
	(verify-type (lambda (record type) (>= (vector-length record) 2)(equal? (vector-ref record 1) type))))
    
    ((foreign-syntax LOC_MACRO "macro") (name constructor predicate . fields)

     (let* ((record-type (gensym "record-type_"))
	    (constructor-name (car constructor))
	    (field-tags (cdr constructor))
	    (counter 2)
	    (ref-fields '())
	    (constructor-statements `((vector-set! v 1 ',record-type)
				      (vector-set! v 0 ',name))))

       (for-each (lambda (field)
		   (if (not (pair? field)) (error "Record definition error - improper field" field))
		   (set! ref-fields (cons (cons (car field) (cons counter (cdr field)))
					  ref-fields))
		   (set! counter (+ counter 1)))
		 fields)

       (for-each (lambda (tag)
		   (let ((field (assoc tag ref-fields)))
		     (if (not field) (error "Record definition error - unspecified field in constructor" tag))
		     (set! constructor-statements (cons `(vector-set! v ,(cadr field) ,tag)
							constructor-statements))
		     ))
		 field-tags)

       (let ((constructor (eval `(lambda ,field-tags
				   (let ((v (make-vector ,counter #f)))
				     ,@(reverse constructor-statements))))))
	 (environment-define! (expansion-environment) constructor-name constructor))

       (for-each (lambda (ref-field)
		   (if (pair? (cddr ref-field))
		       (let ((accessor-name (car (cddr ref-field)))
			     (accessor (eval `(lambda (v)
						(if (verify-type v ',record-type)
						    (vector-ref v ,(cadr ref-field))
						    (error "Record access error - inappropriate type" v))))))
			 (environment-define! (expansion-environment) accessor-name accessor)))

		   (if (pair? (cdr (cddr ref-field)))
		       (let ((mutator-name (cadr (cddr ref-field)))
			     (mutator (eval `(lambda (v o)
					       (if (verify-type v ',record-type)
						   (vector-set! v ,(cadr ref-field) o)
						   (error "Record access error - inappropriate type" v))
					       o))))
			 (environment-define! (expansion-environment) mutator-name mutator))))

		 ref-fields)))))


(define error (foreign-operation LOC_ERROR))
(define error-object-message (foreign-operation LOC_ERRMSG0))
(define error-object? (foreign-operation LOC_ERROBJP))


(define guard 
  (syntax-lambda exp-env (test . body)
   (let* ((eval (foreign-operation LOC_PEVAL))
	  (current-exception-handlers (foreign-operation LOC_CURR_XHANDS))
	  (saved-handlers (current-exception-handlers))
	  (variable (car test))
	  (clauses (cdr test))

	  (gensym (foreign-operation LOC_GENSYM))
	  (generated-symbol (gensym "guard_"))
      	  )
     
     `',(call/cc (lambda (normal-return)

		   (define-values (obj back-to-thunk) (call/cc (lambda (goto-cond)
								    (let ((handler (lambda (obj)
										     (call/cc (lambda (back-to-thunk)
												(goto-cond obj back-to-thunk)))
										     (raise-continuable obj)
										     )))

								      (dynamic-wind 
									  (lambda ()
									    (current-exception-handlers (cons handler saved-handlers)))

									  (lambda ()
									    (normal-return (for-each (lambda (statement)
												       (eval statement exp-env))
												     body)))
									  
									  (lambda ()
									    (current-exception-handlers saved-handlers)))))))
	
		   ((eval `(lambda (,variable ,generated-symbol)
			     ,(cons 'cond (append clauses `((else (,generated-symbol)))))
			    )
			 exp-env) obj back-to-thunk
			 )
		   )))))


(define include
  (let* ((current-directory (foreign-function ff_current_directory))
	 (current-source (foreign-operation LOC_CURR_SOURCE))

	 (macro (foreign-syntax LOC_MACRO "macro"))

	 (path-make-absolute (foreign-function ff_path_make_absolute))
	 (path-directory (foreign-function ff_path_directory))

	 (with-input-from-file (foreign-operation LOC_WITH_INFILE0))
	 (eval (foreign-operation LOC_PEVAL))
	 (read (foreign-operation LOC_READ))
	 )

    (macro filenames
      (let ((result #f))
	(for-each (lambda (filename)
		    (parameterize ((current-source (path-make-absolute filename (path-directory (current-source)))))
				  (with-input-from-file (current-source) 
				    (lambda()
				      (let eval-statement ((statement (read)))
				        (cond ((not (eof-object? statement))
					       (set! result (eval statement (expansion-environment)))
					       (eval-statement (read)))))
				      ))))
		  filenames)
	result))))


;; (make-parameter init converter) MULTI THREADED
;; sets: needs to create a modification table that records thread ID and time
;; gets: need to consult modification table and main thread table and return correct value

(define make-parameter
  (lambda (init . rest)
    (let* ((converter (if (null? rest) #f (car rest)))
	   (value (if converter (converter init) init)))

      (lambda args
	(cond ((null? args) value)
	      ((and (pair? (cdr args))(not (cadr args))) (set! value (car args)))
	      (converter (set! value (converter (car args))))
	      (else (set! value (car args))))))))


(define parameterize 
  ((foreign-syntax LOC_MACRO "macro") (associations . body)
   (let* ((eval (foreign-operation LOC_PEVAL))
	  (params (map (lambda (association)
			 (eval (car association) (expansion-environment)))
		       associations))
	  (tmp-values (map (lambda (association)
			     (eval (cadr association) (expansion-environment)))
			   associations))

	  (prev-values (map (lambda (param)
			      (apply param '()))
			    params)))

     (dynamic-wind
	 (lambda ()
	   (for-each (lambda (param value)
		       (param value)
		       )
		     params tmp-values))

	 (lambda ()
	   (for-each (lambda (statement)
		       (eval statement (expansion-environment)))
		     body))

	 (lambda ()
	   (for-each (lambda (param value)
		       (param value)
		       )
		     params prev-values))))))


(define raise (foreign-operation LOC_RAISE0))


(define raise-continuable 
  (lambda (obj)
    (let* ((current-exception-handlers (foreign-operation LOC_CURR_XHANDS))
	   (handler-list (current-exception-handlers))
	   (current-handler (car handler-list)))
      
      (parameterize ((current-exception-handlers (cdr handler-list)))
		    (current-handler obj)
		    ))))


(define read-string 
  (letrec ((r7-read-string (lambda (k . rest)
			     (if (= k 0) ""
				 (let read-chars ((s (make-string k))
						  (i 0)
						  (c (apply read-char rest)))
				   
				   (cond ((and (eof-object? c) (= i 0)) c)
					 ((eof-object? c) (substring s 0 (+ i 1)))
					 ((< i (- k 1)) (string-set! s i c) (read-chars s (+ i 1) (apply read-char rest)))
					 (else (string-set! s i c) (substring s 0 (+ i 1))))))))

	   (auto-read-string (lambda args
			       (let ((k (if (null? args) #f (car args)))
				     (rest (if (pair? args) (cdr args) '())))
				 (if (not k)
				     (let ((s (apply r7-read-string (cons 64 rest))))
				       (if (or (eof-object? s) (zero? (string-length s))) ""
					   (string-append s (apply auto-read-string (cons #f rest)))))
				     (apply r7-read-string args))))))

    auto-read-string))


(define with-exception-handler 
  ((foreign-syntax LOC_MACRO "macro") (handler-code thunk-code)
   
   (let* ((eval (foreign-operation LOC_PEVAL))
	  (handler (eval handler-code (expansion-environment)))
	  (thunk (eval thunk-code (expansion-environment)))
	  (current-exception-handlers (foreign-operation LOC_CURR_XHANDS)))

     (parameterize ((current-exception-handlers (cons handler (current-exception-handlers))))
		   (thunk)))))


