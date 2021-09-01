;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme base)
  (import (only (s7) + - * / < <= = > >= append apply assoc boolean? car caar cadr call-with-current-continuation call/cc cdar cddr cdr char->integer char=? char? close-input-port close-output-port cons dynamic-wind eof-object? equal? error for-each integer? length make-string map member min negative? newline not null? number? number->string pair? positive? read-string real? reverse string string->symbol string-append string-length string=? string? substring symbol->string symbol? values vector-ref zero?))

  (export            + - * / < <= = > >= append apply assoc boolean? car caar cadr call-with-current-continuation call/cc cdar cddr cdr char->integer char=? char? close-input-port close-output-port cons dynamic-wind eof-object? equal? error for-each integer? length make-string map member min negative? newline not null? number? number->string pair? positive? read-string real? reverse string string->symbol string-append string-length string=? string? substring symbol->string symbol? values vector-ref zero?)


  (import (only (s7) 
		_begin
		_list
		_if
		_cond
		_else
		_and
		rootlet
		))

  (export (rename _begin begin)
	  (rename _list list) 
	  (rename _if if)
	  (rename _cond cond)
	  (rename _else else)
	  (rename _and and)
	  
	  )

  


  (export vector-for-each
	  (rename _include include)
	  )


  (begin

    (define read-string 
      (let ((s7_read-string read-string))
	(lambda args
	  (let ((k (if (null? args) #f (car args)))
		(rest (if (pair? args) (cdr args) '()))
		)
	    (if (not k)
		(let ((s (apply s7_read-string (cons 64 rest))))
		  (if (or (eof-object? s) (zero? (string-length s)))
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


    (define _include 
      (begin 
	(environment-update! (rootlet) '*source-path* #f)

	(macro files
	  (import (auto scheme path))
	  (display "including files: ")(write files)(newline)
	  (let* ((prev_*source-path* *source-path*)

		 (included-source (path-make-absolute (car files) (path-directory *source-path*)))
		 (included-expressions (with-input-from-file included-source read-list))
		 )
	    
	    ;; (cons 'begin included-expressions)
	    

	    `(dynamic-wind 
		 (lambda () (environment-update! (rootlet) '*source-path* ,included-source)
		    (display "inside before")(newline))

		 (lambda () (with-let (outlet (current-environment)) ,@included-expressions))

		 (lambda () (environment-update! (rootlet) '*source-path* ,prev_*source-path*)
		    (display "inside after")(newline))
	       )


	    )
	  )
	)
      )
    )

  )