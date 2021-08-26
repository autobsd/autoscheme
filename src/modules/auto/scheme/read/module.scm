;; (scheme read)
(define-library (auto scheme read)

  (export read read-list)

  (import (only (s7) read))

  (begin

    (define read-list
      (lambda args
	(reverse (let read-expressions ((expressions '()))
		   (let ((next-expression (apply read args)))
		     (if (eof-object? next-expression) expressions
			 (read-expressions (cons next-expression expressions))))))))
    
    ))
