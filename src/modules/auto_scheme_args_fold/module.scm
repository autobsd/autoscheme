;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme args fold)
  
  (import (auto scheme base)
	  (auto scheme environment)
	  (auto scheme write)
	  )

  (export option option-names option-required-arg? option-optional-arg? option-processor args-fold)

  (begin

    (define option 
      (lambda (names required-arg? optional-arg? processor)

	(apply make-environment `((names . ,names)
				  (required-arg? . ,required-arg?)
				  (optional-arg? . ,optional-arg?)
				  (processor . ,processor)

				  )
	       )))

    (define option-names (lambda (option) (environment-ref option 'names)))
    (define option-required-arg? (lambda (option) (environment-ref option 'required-arg?)))
    (define option-optional-arg? (lambda (option) (environment-ref option 'optional-arg?)))
    (define option-processor (lambda (option) (environment-ref option 'processor)))

    (define args-fold-remainder
      (lambda (remainder options unrecognized-option-proc operand-proc end-of-options . seeds)

	(letrec* ((argument (if (pair? remainder) (car remainder) #f))
		  (argument-length (if argument (string-length argument)))

		  (find-option (lambda (name) (call/cc (lambda (return) (for-each (lambda (opt) (if (member name (option-names opt)) (return opt))) options)(return #f)))))

		  (process-option (lambda ()
				    (cond ((equal? (string-ref argument 1) #\-) (process-long-option)) (else (process-short-option)))))

		  (process-short-option (lambda ()
					  (let* ((opt-name (string-ref argument 1))
						 (recognized-option (find-option opt-name))
						 (opt-arg-allowed (or (not recognized-option) (option-required-arg? recognized-option) (option-optional-arg? recognized-option)))
						 (opt-arg-string (if (> argument-length 2) (substring argument 2 (string-length argument)) #f))

						 (updated-remainder (cdr remainder))

						 (opt-arg (cond ((not opt-arg-string)
								 (cond ((and opt-arg-allowed (pair? (cdr remainder)) (not (equal? (string-ref (cadr remainder) 0) #\-)))
									(set! updated-remainder (cddr remainder)) (cadr remainder))
								       (else #f)))

								((or (not opt-arg-allowed) (find-option (opt-arg-string 0)))
								 (set! updated-remainder (cons (string-append "-" opt-arg-string) (cdr remainder))) #f)

								(else opt-arg-string)
								))
						 
						 (result (call-with-values (lambda () (if recognized-option 
										     (apply (option-processor recognized-option) (append (list recognized-option opt-name opt-arg) seeds))
										     (apply unrecognized-option-proc (append (list recognized-option opt-name opt-arg) seeds))
										     )) list))
						 )

					    (apply args-fold-remainder (append (list updated-remainder options unrecognized-option-proc operand-proc end-of-options) result))
					    )
					  
					  ))

		  (process-long-option (lambda ()
					 (let* ((name-end (let find-end ((pos 2))
							    (cond ((equal? (string-length argument) pos) pos)
								  ((equal? (string-ref argument pos) #\=) pos)
								  (else (find-end (+ pos 1))))))
						(opt-name (substring argument 2 name-end))
						(recognized-option (find-option opt-name))
						(opt-arg-string (if (< name-end (string-length argument))
								    (substring argument (+ name-end 1) (string-length argument))
								    #f))

						(updated-remainder (cdr remainder))
						(opt-arg opt-arg-string)
						
						(result (call-with-values (lambda () (if recognized-option 

										    (apply (option-processor recognized-option) (append (list recognized-option opt-name opt-arg) seeds))

										    (apply unrecognized-option-proc (append (list recognized-option opt-name opt-arg) seeds))
										    )
									     ) list
									       ))

						)

					   (apply args-fold-remainder (append (list updated-remainder options unrecognized-option-proc operand-proc end-of-options) result ))
					   )))
		  
		  (process-operand (lambda ()
				     (let ((result (call-with-values (lambda () (apply operand-proc (append (list argument) seeds)) ) list)))
				       
				       (apply args-fold-remainder (append (list (cdr remainder) options unrecognized-option-proc operand-proc end-of-options) result))
				       )))
		  

		  )

		 (cond ((not argument) (values (car seeds) (cadr seeds)))
		       (end-of-options (process-operand))
		       ((equal? argument "--") (apply args-fold-remainder (append (list (cdr remainder) options unrecognized-option-proc operand-proc #t) seeds)) )
		       ((<= (string-length argument) 1) (process-operand))
		       ((equal? (string-ref argument 0) #\-) (process-option))
		       (else (process-operand))
		       )
		 
		 )))


    (define args-fold
      (lambda (args options unrecognized-option-proc operand-proc . seeds)

	(apply args-fold-remainder (append (list args options unrecognized-option-proc operand-proc #f) seeds))

	))

    ))