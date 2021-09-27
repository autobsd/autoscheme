;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

;; (foreign-declaration (include-string "declarations.h"))
;; (foreign-definition (include-string "definitions.c"))
;; (foreign-initialization (include-string "initialization.c"))



;; (define-library (auto scheme process context)
;;   (import (only (s7) exit emergency-exit))
;;   (export exit emergency-exit)

;;   (export command-line current-directory)
;;   )

;; (let ()
;;   (import (only (s7) rootlet))
;;   (environment-remove! (rootlet) 'command-line 'current-directory))


(define-library (auto scheme process context)
  (import (only (auto scheme)
		emergency-exit
		exit

		begin
		))
  (export emergency-exit
	  exit
	  )
  )
  
