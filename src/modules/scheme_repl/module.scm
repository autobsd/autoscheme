;;  This file is part of the 'AutoScheme' project.
;;  Copyright (c) 2026 AutoBSD Contributors.
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (scheme repl)

  (import (scheme eval)
	  )

  (export interaction-environment)

  (begin
    
    ((foreign-syntax LOC_DEF0 "define") interaction-environment 
      ((foreign-syntax LOC_LAMBDA "lambda") ()
	(environment (auto scheme base))
	))

    ))
