(import (scheme write))
(import (scheme base))
(display "AutoScheme")(newline)


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


(define args-fold
  (lambda (args options unrecognized-option-proc operand-proc . seeds)


    (letrec* ((process-remainder (lambda (remainder end-of-options . seeds)
				   (cond ((null? remainder) (apply values seeds))

					 ((equal? (car remainder) "--") (process-remainder (cdr remainder) #t))

					 (end-of-options (process-operand remainder))

					 ((<= (string-length (car remainder)) 1)(process-operand remainder))
					 ((not (equal? ((car remainder) 0) #\-)) (process-operand remainder))

					 ((equal? ((car remainder) 0) #\-) (process-option))
					 (else (display "unknow condition")(newline))
					 )
				   ))

	      (process-option (lambda (remainder)
				(cond ((not (equal? ((car remainder) 0) #\-)) (display "found an option: ")(write (car remainder))(newline))
				      (else ))
				(apply values seeds)))

	      (process-operand (lambda (remainder)
				 (display "found an operand: ") (write remainder)(newline)
				 (cond ((not (equal? ((car remainder) 0) #\-)) (apply operand-proc (cons (car remainder) seeds)))

				       (else (apply operand-proc (cons (car remainder) seeds))))
				 (apply values seeds)))

	      )
	     ;; 
	     (process-remainder args #f)
	     )))


(define help-option
  (option 
   '(#\h "help") #f #f
   (lambda args
     (display "-help message-")(newline)
     (exit) )))

(define unrecognized-processor 
  (lambda (option name arg . seeds)
    (display "unrecognized argument")(newline)
    (exit) ))


(define operand-processor 
  (lambda (operand . seeds)
    (display "inside operand processor")(newline)
    (exit) ))


(display "args-fold: ")
(write (list
  (args-fold (cdr (command-line) )
	     `(help-option)
	     unrecognized-processor
	     operand-processor
	     0 "seed2"
	     ))
       )
(newline)


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