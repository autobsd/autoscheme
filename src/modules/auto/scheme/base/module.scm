;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-define (include-string "definitions.c"))
(foreign-initialize (include-string "initialization.c"))





(define read-string 
  (letrec ((r7-read-string (lambda (k . rest)
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


	   (auto-read-string (lambda args
			       (let ((k (if (null? args) #f (car args)))
				     (rest (if (pair? args) (cdr args) '()))
				     )
				 (if (not k)
				     (let ((s (apply r7-read-string (cons 64 rest))))
				       (if (or (eof-object? s) (zero? (string-length s)))
					   ""
					   (string-append s (apply auto-read-string (cons #f rest)))))
				     (apply r7-read-string args))
				 )))
	   )
    auto-read-string
    ))
