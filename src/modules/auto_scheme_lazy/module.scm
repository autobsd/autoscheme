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

    ((foreign-syntax LOC_DEF0 "define") delay (foreign-syntax LOC_DELAY "delay"))
    ((foreign-syntax LOC_DEF0 "define") force (foreign-operation LOC_FORCE))

    ;; ((foreign-syntax LOC_DEF0 "define") lazy (foreign-operation LOC_LAZY))
    ;; ((foreign-syntax LOC_DEF0 "define") eager (foreign-operation LOC_EAGER))
    ;; ((foreign-syntax LOC_DEF0 "define") cons-stream (foreign-operation LOC_C0STREAM))
  )
)


