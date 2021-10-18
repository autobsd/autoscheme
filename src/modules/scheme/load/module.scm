;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (scheme load)

  (import (auto scheme base)
	  (auto scheme file)
	  (scheme eval)
	  (scheme read)
	  )

  (export load)

  (begin

     (define load
       (lambda (filename . rest)

	 (define load-environment (if (null? rest) 
				      (interaction-environment)
				      (car rest)))

	 (with-input-from-file filename
	   (lambda ()
	     (let load-expression ((expression (read)))
	       (cond ((not (eof-object? expression))
		      (eval expression load-environment)
		      (load-expression (read))))
	       )

	     ))
	 ))
     ))