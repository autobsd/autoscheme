;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause


(define-library (auto scheme interpret)

  (import (auto scheme base)
	  (auto scheme write)
	  (auto scheme environment)
	  (scheme eval)
	  (scheme load)
	  (auto scheme file)
	  (auto scheme read)
	  )
  (export interpret
	  ;; interpretation-environment
	  )
  (begin

  
     
     (define interpret
       (lambda sources

	 (define int-env (environment))

	 (for-each (lambda (source)
		     (load source int-env))
		   sources)
	 ))

     ))