;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme char-set)

  (import (auto scheme base) 

	  )

  (export char-set
	  char-set?
	  char-set-contains?

	  char-set:blank 
	  char-set:whitespace 
	  )

  (begin 

    
    (define-record-type <char-set>
      (make-char-set chars contains?)
      char-set?
      (chars get-chars)
      (contains? get-contains)
      )

    (define char-set:contains?
      (lambda (cs c) 
	(if (member c (get-chars cs)) #t #f)))




    (define char-set
      (lambda chars
	(make-char-set chars
		       char-set:contains?)))
    
    (define char-set-contains?
      (lambda (cs c)
	((get-contains cs) cs c)))


    (define char-set:blank (char-set #\space #\tab))
    (define char-set:whitespace (char-set #\space #\newline #\tab #\x0C #\x0B #\return))
  
    ))