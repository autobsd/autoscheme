;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

;; (auto scheme list)
(define alist-delete
  (lambda (alist key)
    (cond ((null? alist) '())
	  ((equal? (caar alist) key) (alist-delete (cdr alist) key))
	  (else (cons (car alist) (alist-delete (cdr alist) key))))))
