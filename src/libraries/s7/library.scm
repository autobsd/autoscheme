;; (set! (*s7* 'print-length) 1024)

(define s7_object->string object->string)
(define object->string
  (lambda (obj)
    (let ((str (s7_object->string obj)))
      (if (string? obj) 
	  (with-output-to-string (lambda ()
				   (do ((i 0 (+ i 1)))
				       ((= i (string-length str)) )      
				     (cond ((member (str i) `(,("\n" 0) #\newline))(display "\\n"))
					   (else (display (str i)))))))
	  str
	  ))))

(define-library (s7)

  (export + - * / < <= = > >=)                           
  (export caaaar)
  (export eval)
  (export acos)
  (export load)
  (export read)
  (export display write)
  (export caaaar caaar caaddr cadaar cadar cadddr cdaaar cdaar cdaddr cddaar cddar cddddr
	caaadr caadar caadr cadadr caddar caddr cdaadr cdadar cdadr cddadr cdddar cdddr)
  )


(cutlet (rootlet) '+ '- '* '/ '< '<= '= '> '>=)
(cutlet (rootlet) 'caaaar)
(cutlet (rootlet) 'eval)
(cutlet (rootlet) 'acos)
(cutlet (rootlet) 'read)
(cutlet (rootlet) 'load)
(cutlet (rootlet) 'display 'write)
(cutlet (rootlet) 'caaaar 'caaar 'caaddr 'cadaar 'cadar 'cadddr 'cdaaar 'cdaar 'cdaddr 'cddaar 'cddar 'cddddr
	'caaadr 'caadar 'caadr 'cadadr 'caddar 'caddr 'cdaadr 'cdadar 'cdadr 'cddadr 'cdddar 'cdddr)
