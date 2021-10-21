;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme lazy)

  (export delay
	  force

	  ;; lazy
	  ;; eager
	  ;; cons-stream
	  )

  (begin

    ((foreign-syntax LOC_DEF0 "define") delay (foreign-procedure LOC_DELAY))
    ((foreign-syntax LOC_DEF0 "define") force (foreign-procedure LOC_FORCE))

    ;; ((foreign-syntax LOC_DEF0 "define") lazy (foreign-procedure LOC_LAZY))
    ;; ((foreign-syntax LOC_DEF0 "define") eager (foreign-procedure LOC_EAGER))
    ;; ((foreign-syntax LOC_DEF0 "define") cons-stream (foreign-procedure LOC_C0STREAM))
  )
)


