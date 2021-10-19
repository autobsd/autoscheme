;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme inexact)

  (export acos
	  asin
	  atan
	  cos
	  exp
	  log
	  sin
	  sqrt
	  tan
	  )

  (begin

    ((foreign-syntax OP_DEF0 "define") acos (foreign-procedure OP_ACOS))
    ((foreign-syntax OP_DEF0 "define") asin (foreign-procedure OP_ASIN))
    ((foreign-syntax OP_DEF0 "define") atan (foreign-procedure OP_ATAN))
    ((foreign-syntax OP_DEF0 "define") cos (foreign-procedure OP_COS))
    ((foreign-syntax OP_DEF0 "define") exp (foreign-procedure OP_EXP))
    ((foreign-syntax OP_DEF0 "define") log (foreign-procedure OP_LOG))
    ((foreign-syntax OP_DEF0 "define") sin (foreign-procedure OP_SIN))
    ((foreign-syntax OP_DEF0 "define") sqrt (foreign-procedure OP_SQRT))
    ((foreign-syntax OP_DEF0 "define") tan (foreign-procedure OP_TAN))
    
  )
)


