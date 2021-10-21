;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme closure)

  (export make-closure
	  get-closure-code
	  closure?
	  )

  (begin

    ((foreign-syntax LOC_DEF0 "define") make-closure (foreign-procedure LOC_MKCLOSURE))
    ((foreign-syntax LOC_DEF0 "define") get-closure-code (foreign-procedure LOC_GET_CLOSURE))
    ((foreign-syntax LOC_DEF0 "define") closure? (foreign-procedure LOC_CLOSUREP))

  )
)


