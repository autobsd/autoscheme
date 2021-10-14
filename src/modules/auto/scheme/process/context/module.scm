;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-define (include-string "definitions.c"))
(foreign-initialize (include-string "initialization.c"))


(define-library (auto scheme process context)
  (import (only (auto scheme base)
		emergency-exit
		exit
		quote
		begin
		)
	  (auto scheme environment)
	  )
  (export (rename _command-line command-line)
	  (rename _current-directory current-directory)
	  emergency-exit
	  exit
	  )
  

  (begin
    (environment-update! (current-environment) '_command-line (environment-ref (global-environment) 'command-line))
    (environment-delete! (global-environment) 'command-line)

    (environment-update! (current-environment) '_current-directory (environment-ref (global-environment) 'current-directory))
    (environment-delete! (global-environment) 'current-directory)
    )
  )


