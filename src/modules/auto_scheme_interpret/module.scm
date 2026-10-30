;;  This file is part of the 'AutoScheme' project.
;;  Copyright (c) 2026 AutoBSD Contributors.
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme interpret)

  (import (auto scheme base)
	  (scheme eval)
	  (scheme load)
	  (scheme process-context)
	  )

  (export interpret
	  )

  (begin
     
     (define interpret
       (lambda (sources arguments)

	 (define int-env (environment))
	 
		       (for-each (lambda (source)
				   (parameterize ((command-line (cons source arguments)))
						 (load source int-env)))
				 sources)
		       ))

     ))