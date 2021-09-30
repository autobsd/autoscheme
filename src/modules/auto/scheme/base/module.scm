;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme base)
  (import  (auto scheme))
		;; * + / < <= = => > >= - abs and append apply assoc assq assv begin boolean? caar cadr call-with-current-continuation call-with-values car case cdar cddr cdr ceiling char->integer char-ready? char<=? char<? char=? char>=? char>? char? close-input-port close-output-port close-port cond cons current-input-port current-output-port define define-syntax do dynamic-wind else eof-object? eq? equal? eqv? error even? exact? expt exact->inexact floor for-each gcd get-output-string if inexact->exact inexact? input-port? integer->char integer? lambda lcm length let let* let-syntax letrec letrec* letrec-syntax list list->string list->vector list-ref list-tail list? make-string make-vector map max member memq memv min modulo negative? newline not null? number->string number? odd? open-input-output-string open-input-string open-output-string or output-port? pair? peek-char port? positive? procedure? quasiquote quote quotient read-char real? remainder reverse round set! set-car! set-cdr! string string->list string->number string->symbol string-append string-copy string-fill! string-length string-ref string-set! string<=? string<? string=? string>=? string>? string? substring symbol->string symbol? syntax-rules truncate unless values vector vector->list vector-fill! vector-length vector-ref vector-set! vector? when write-char zero?
		;; ))

  (export       * + / < <= = => > >= - abs and append apply assoc assq assv begin boolean? caar cadr call-with-current-continuation call-with-values car case cdar cddr cdr ceiling char->integer char-ready? char<=? char<? char=? char>=? char>? char? close-input-port close-output-port close-port cond cons current-input-port current-output-port define define-syntax do dynamic-wind else eof-object? eq? equal? eqv? error even? exact? expt exact->inexact floor for-each gcd get-output-string if inexact->exact inexact? input-port? integer->char integer? lambda lcm length let let* let-syntax letrec letrec* letrec-syntax list list->string list->vector list-ref list-tail list? make-string make-vector map max member memq memv min modulo negative? newline not null? number->string number? odd? open-input-output-string open-input-string open-output-string or output-port? pair? peek-char port? positive? procedure? quasiquote quote quotient read-char real? remainder reverse round set! set-car! set-cdr! string string->list string->number string->symbol string-append string-copy string-fill! string-length string-ref string-set! string<=? string<? string=? string>=? string>? string? substring symbol->string symbol? syntax-rules truncate unless values vector vector->list vector-fill! vector-length vector-ref vector-set! vector? when write-char zero?
		)
  (export (rename call-with-current-continuation call/cc))
  (export (rename auto-read-string read-string))

  (begin

    (define read-string
      (lambda (k . rest)
	(if (= k 0) ""
	    (let read-chars((s (make-string k))
			    (i 0)
			    (c (apply read-char rest))
			    )
	      
	      (cond ((and (eof-object? c) (= i 0)) c)
		    ((eof-object? c) (substring s 0 (+ i 1)))

		    ((< i (- k 1)) (string-set! s i c) (read-chars s (+ i 1) (apply read-char rest)))
		    
		    (else (string-set! s i c) (substring s 0 (+ i 1))))
	      ))))
	

    (define auto-read-string 
      (lambda args
	(let ((k (if (null? args) #f (car args)))
	      (rest (if (pair? args) (cdr args) '()))
	      )
	  (if (not k)
	      (let ((s (apply read-string (cons 64 rest))))
		(if (or (eof-object? s) (zero? (string-length s)))
		    ""
		    (string-append s (apply auto-read-string (cons #f rest)))))
	      (apply read-string args))
	  )))



    )
  )