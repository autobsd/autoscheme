;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme macro)

  (export define-macro
	  macro
	  macro-expand
	  macro?

	  gensym
	  )

  (begin

    ((foreign-syntax LOC_DEF0 "define") define-macro (foreign-syntax LOC_DEFMACRO0 "define-macro"))
    ((foreign-syntax LOC_DEF0 "define") macro (foreign-syntax LOC_MACRO "macro"))
    ((foreign-syntax LOC_DEF0 "define") macro-expand (foreign-operation LOC_MACRO_EXPAND0))
    ((foreign-syntax LOC_DEF0 "define") macro? (foreign-operation LOC_MACROP))

    ((foreign-syntax LOC_DEF0 "define") gensym (foreign-operation LOC_GENSYM))
    ))
