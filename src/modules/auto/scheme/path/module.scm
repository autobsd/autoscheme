;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-define (include-string "definitions.c"))


(define-library (auto scheme path)

  (import (auto scheme base)
	  (auto scheme directory)
	  )

  (export path-directory
	  path-absolute?
	  path-make-absolute
	  )

  (begin 

    (define path-directory
      (lambda (path)
	(let ((pos -1)
	      )
	  (do ((len (string-length path))
	       (i 0 (+ i 1)))
	      ((= i len))
	    (if (char=? (string-ref path i) #\/)
		(set! pos i)))

	  (if (positive? pos) 
	      (substring path 0 pos)))))


    (define path-absolute? (foreign-function ff_path_absolute_p))



    (define path-make-absolute
      (lambda (path . rest)
	(if (path-absolute? path) 
	    path
	    (let ((parent-dir (if (pair? rest) 
				  (car rest)
				  (current-directory)))
		  )
	      (string-append parent-dir "/" path)))))
    
    ))