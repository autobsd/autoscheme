(import (scheme write))
(import (scheme base))



;; (define fold-left 
;;   (lambda (f init seq) 
;;     (if (null? seq) 
;; 	init 
;; 	(fold-left f 
;; 		   (f init (car seq)) 
;; 		   (cdr seq)))))

;; (define fold-right 
;;   (lambda (f init seq) 
;;     (if (null? seq) 
;; 	init 
;; 	(f (car seq) 
;; 	   (fold-right f init (cdr seq))))))

;; (define fold fold-left)

(define option 
  (lambda (names required-arg? optional-arg? processor)
    (inlet 'names names
	   'required-arg? required-arg?
	   'optional-arg? optional-arg?
	   'processor processor)
    ))

(define option-names (lambda (option) (let-ref option 'names)))
(define option-required-arg? (lambda (option) (let-ref option 'required-arg?)))
(define option-optional-arg? (lambda (option) (let-ref option 'optional-arg?)))
(define option-processor (lambda (option) (let-ref option 'processor)))

(define args-fold-remainder
  (lambda (remainder options unrecognized-option-proc operand-proc end-of-options . seeds)

    (letrec* ((argument (if (pair? remainder) (car remainder) #f))
	      (argument-length (string-length argument))

	      (find-option (lambda (name) (call/cc (lambda (return) (for-each (lambda (opt) (if (member name (option-names opt)) (return opt))) options)(return #f)))))

	      (process-option (lambda () (cond ((equal? (argument 1) #\-) (process-long-option)) (else (process-short-option)))))

	      (process-short-option (lambda ()
				      (let* ((opt-name (argument 1))
					     (recognized-option (find-option opt-name))
					     (opt-arg-allowed (or (not recognized-option) (option-required-arg? recognized-option) (option-optional-arg? recognized-option)))
					     (opt-arg-string (if (> argument-length 2) (substring argument 2 (string-length argument)) #f))

					     (updated-remainder (cdr remainder))

					     (opt-arg (cond ((not opt-arg-string)
							 (cond ((and opt-arg-allowed (pair? (cdr remainder)) (not (equal? ((cadr remainder) 0) #\-)))
								(set! updated-remainder (cddr remainder)) (cadr remainder))
							       (else #f)))

							((or (not opt-arg-allowed) (find-option (opt-arg-string 0)))
							 (set! updated-remainder (cons (string-append "-" opt-arg-string) (cdr remainder))) #f)

							(else opt-arg-string)
							))
					     
					     (result (if recognized-option 
							 ((option-processor recognized-option) recognized-option opt-name opt-arg (apply values seeds))
							 (unrecognized-option-proc recognized-option opt-name opt-arg (apply values seeds))))
					     )

					(args-fold-remainder updated-remainder options unrecognized-option-proc operand-proc end-of-options (apply values seeds ))
					)
				      
				      ))

	      (process-long-option (lambda ()
				     (let* ((name-end (let find-end ((pos 2))
						       (cond ((equal? (string-length argument) pos) pos)
							     ((equal? (argument pos) #\=) pos)
							     (else (find-end (+ pos 1))))))
					    (opt-name (substring argument 2 name-end))
					    (recognized-option (find-option opt-name))
					    (opt-arg-string (if (< name-end (string-length argument))
								(substring argument (+ name-end 1) (string-length argument))
								#f))

					    )
				       (display "processing long - remainder: ")(write remainder)(newline)
				       (display "opt-name: ")(write opt-name)(newline)
				       (display "opt-arg-string: ")(write opt-arg-string)(newline)


				       )))
	      
	      (process-operand (lambda ()
				 (let ((result (list (operand-proc argument (apply values seeds)))))
				   (args-fold-remainder (cdr remainder) options unrecognized-option-proc operand-proc end-of-options (apply values result )))))
	      

	   )

      (cond ((not argument) (apply values seeds))
	    (end-of-options (process-operand))
	    ((equal? argument "--") (args-fold-remainder (cdr remainder) options unrecognized-option-proc operand-proc #t (apply values seeds)))
	    ((<= (string-length argument) 1) (process-operand))
	    ((equal? (argument 0) #\-) (process-option))
	    (else (process-operand))
	    )
      
      )))


(define args-fold
  (lambda (args options unrecognized-option-proc operand-proc . seeds)
    (args-fold-remainder args options unrecognized-option-proc operand-proc #f (apply values seeds))))


(define display-version
  (lambda ()
    (display (string-append "AutoScheme version " "0.31.0 (rev 1627370131)"))(newline)
    ))


(define display-usage
  (lambda (opt-list)
    (display "Usage: autoscheme [options...] [sources...]")(newline)
    
    ;; (write opt-list)(newline)
    ))


(define help-option
  (option 
   '(#\h "help") #f #f
   (lambda args
     (display-version)
     (display-usage option-list)

     (exit) )))

(define version-option
  (option 
   '(#\V "version") #f #f
   (lambda args
     (display-version)
     (exit) )))

(define option-list (list help-option 
			  version-option))




(define unrecognized-processor 
  (lambda (option name arg . seeds)
    (let ((name-string (if (char? name) 
			   (string-append "-" (string name))
			   (string-append "--" name)))
	  )

      (display (string-append "autoscheme: unrecognized option " name-string))(newline)
      ;; (display (string-append "autoscheme: try 'autoscheme --help' for more information"))(newline)
      (display-usage option-list)
      (exit 1) )))


(define operand-processor 
  (lambda (operand . seeds)
    (let ((options (car seeds))
	  (source-files (cadr seeds)))

      (values options (cons operand source-files)))))



(let* ((seeds (list (inlet) '()))
       (result (list (args-fold (cdr (command-line) )
				option-list
				unrecognized-processor
				operand-processor
				(apply values seeds)
				)))
       (options (car result))
       (source-files (reverse (cadr result)))
       )
  (display "options: ")(write options)(newline)
  (display "source-files: ")(write source-files)(newline)
  )
  



(newline)

;; (define get-argument 
;;   (lambda (args flag)
;;     (call/cc (lambda (return)
;; 	       (let process-remainder ((remainder args))
;; 		 (cond ((null? remainder) #f)
;; 		       ((equal? (car remainder) flag) (return (if (and (pair? (cdr remainder))
;; 								       (not (equal? ((cadr remainder) 0) "-")))
;; 								  (cadr remainder)
;; 								  #f)))
;; 		       (else (process-remainder (cdr remainder)))))))))

;; (define has-flag? 
;;   (lambda (args flag)
;;     (call/cc (lambda (return)
;; 	       (let process-remainder ((remainder args))
;; 		 (cond ((null? remainder) #f)
;; 		       ((equal? (car remainder) flag) (return #t))
;; 		       (else (process-remainder (cdr remainder)))))))))


;; (define compile
;;   (lambda (args)
;;     (display "compiling with args: ")(write args)(newline)
;;     (display "source-file: ")(write (get-argument args "-c"))(newline)
;;     (display "module-name: ")(write (get-argument args "-m"))(newline)
;;     ))


;; (let ((args (cdr (command-line))))
;;   (cond ((has-flag? args "-c") (compile args))
;; 	(else )
;; 	))


;; (define test-proc
;;   (lambda ()
;;     (values 5 3)))
;; (display "test-proc: ")(write (call-with-values test-proc (lambda args (cadr args))))(newline)