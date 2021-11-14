;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme list)

  (import (auto scheme base))

  (export alist?
	  alist-delete 
	  alist-delete!

	  last-pair
	  )

  (begin 

    (define alist?
      (lambda (obj)
	(cond ((null? obj) #t)
	      ((and (pair? obj) (pair? (car obj))) (alist? (cdr obj)))
	      (else #f))))

    
    (define alist-delete
      (lambda (alist key)
	(cond ((null? alist) '())
	      ((equal? (caar alist) key) (alist-delete (cdr alist) key))
	      (else (cons (car alist) (alist-delete (cdr alist) key))))))


    (define alist-delete!
      (lambda (alist key)

	(cond ((null? alist) '())

	      ((equal? (caar alist) key) (cond ((null? (cdr alist)) '())
					       (else (set-car! alist (cadr alist))
						     (set-cdr! alist (cddr alist))
						     (alist-delete alist key)
						     alist)
						     ))

	      (else (set-cdr! alist (alist-delete (cdr alist) key)) alist)

	      )
	))


    (define last-pair (foreign-operation LOC_LASTPAIR))



    ))