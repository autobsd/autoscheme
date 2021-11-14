;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme sort)
  
  (import (auto scheme base)
	  )

  (export list-sort)

  (begin
    
    (define list-sort
      (lambda (< unsorted-list)
	(let ((sorted-list '())
	      )

	  (define add-to-list
	    (lambda (item sorted-list)

	      (cond ((null? sorted-list) (cons item sorted-list))
		    ((< item (car sorted-list)) (cons item sorted-list))
		    (else (cons (car sorted-list) (add-to-list item (cdr sorted-list)))))
	      ))

	  (for-each (lambda (item)
		      (set! sorted-list (add-to-list item sorted-list)))
		    unsorted-list)

	  sorted-list
	  )))
    ))