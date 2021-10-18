;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme macro)

  (export define-macro
	  macro
	  macro-expand
	  macro?
	  )

  (begin

    ((foreign-syntax OP_DEF0 "define") define-macro (foreign-syntax OP_DEFMACRO0 "define-macro"))
    ((foreign-syntax OP_DEF0 "define") macro (foreign-syntax OP_MACRO "macro"))
    ((foreign-syntax OP_DEF0 "define") macro-expand (foreign-procedure OP_MACRO_EXPAND0))
    ((foreign-syntax OP_DEF0 "define") macro? (foreign-procedure OP_MACROP))
    ))
