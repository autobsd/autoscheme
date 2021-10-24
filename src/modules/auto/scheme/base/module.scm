;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-define (include-string "definitions.c"))
(foreign-initialize (include-string "initialization.c"))





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



((foreign-syntax LOC_DEFMACRO0 "define-macro") (parameterize associations . body)
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
		    params prev-values)))))



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



  



((foreign-syntax LOC_DEF0 "define") write-simple (foreign-operation LOC_WRITE))


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


	
  

(define raise (foreign-operation LOC_RAISE0))

    