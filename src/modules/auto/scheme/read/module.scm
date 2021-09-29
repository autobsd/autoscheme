;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme read)

  (export read read-list)

  (import (only (auto scheme) read)
	  (auto scheme base)
	  )
	
  (begin

    (define read-list
      (lambda args
	(reverse (let read-expressions ((expressions '()))
		   (let ((next-expression (apply read args)))
		     (if (eof-object? next-expression) expressions
			 (read-expressions (cons next-expression expressions))))))))
    
    ))
