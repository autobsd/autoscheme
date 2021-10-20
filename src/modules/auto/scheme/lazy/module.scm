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

    ((foreign-syntax OP_DEF0 "define") delay (foreign-procedure OP_DELAY))
    ((foreign-syntax OP_DEF0 "define") force (foreign-procedure OP_FORCE))

    ;; ((foreign-syntax OP_DEF0 "define") lazy (foreign-procedure OP_LAZY))
    ;; ((foreign-syntax OP_DEF0 "define") eager (foreign-procedure OP_EAGER))
    ;; ((foreign-syntax OP_DEF0 "define") cons-stream (foreign-procedure OP_C0STREAM))
  )
)


