;; (srfi 1) (scheme list)
(define-library (auto scheme list)

  (export fold fold-right)
  
  (begin

    (define fold-left 
      (lambda (f init seq) 
	(if (null? seq) 
	    init 
	    (fold-left f 
		       (f init (car seq)) 
		       (cdr seq)))))

    (define fold-right 
      (lambda (f init seq) 
	(if (null? seq) 
	    init 
	    (f (car seq) 
	       (fold-right f init (cdr seq))))))

    (define fold fold-left)

    ))