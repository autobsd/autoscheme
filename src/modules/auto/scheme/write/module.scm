;; (scheme write)
(define-library (auto scheme write)

  (export display write)

  (import (s7))

  (begin
    (define s7_write write)
	 (define write
	   (lambda (obj . args)
	     (if (string? obj)
		 (apply display (cons (object->string obj) args))
		 (apply s7_write (cons obj args)))))
	 )
  )
