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

    ((foreign-syntax LOC_DEF0 "define") acos (foreign-operation LOC_ACOS))
    ((foreign-syntax LOC_DEF0 "define") asin (foreign-operation LOC_ASIN))
    ((foreign-syntax LOC_DEF0 "define") atan (foreign-operation LOC_ATAN))
    ((foreign-syntax LOC_DEF0 "define") cos (foreign-operation LOC_COS))
    ((foreign-syntax LOC_DEF0 "define") exp (foreign-operation LOC_EXP))
    ((foreign-syntax LOC_DEF0 "define") log (foreign-operation LOC_LOG))
    ((foreign-syntax LOC_DEF0 "define") sin (foreign-operation LOC_SIN))
    ((foreign-syntax LOC_DEF0 "define") sqrt (foreign-operation LOC_SQRT))
    ((foreign-syntax LOC_DEF0 "define") tan (foreign-operation LOC_TAN))
    
  )
)


