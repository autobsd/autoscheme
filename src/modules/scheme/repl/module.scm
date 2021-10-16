;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (scheme repl)

  (import (scheme eval)
	  )

  (export interaction-environment)

  (begin
    
    ((foreign-syntax OP_DEF0 "define") interaction-environment 
      ((foreign-syntax OP_LAMBDA "lambda") ()
	(environment (auto scheme base))
	))

    ))
