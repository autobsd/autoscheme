;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme car)

  (export char-alphabetic?

	  char-ci<=?
	  char-ci<?
	  char-ci=?
	  char-ci>=?
	  char-ci>?

	  char-downcase
	  char-lower-case?
	  char-numeric?
	  char-upcase
	  char-upper-case?
	  char-whitespace?

	  string-ci<=?
	  string-ci<?
	  string-ci=?
	  string-ci>=?
	  string-ci>?

	  )

  (begin

    ((foreign-syntax LOC_DEF0 "define") char-alphabetic? (foreign-procedure LOC_CHARAP))

    ((foreign-syntax LOC_DEF0 "define") char-ci<=? (foreign-procedure LOC_CHARCILEQ))
    ((foreign-syntax LOC_DEF0 "define") char-ci<? (foreign-procedure LOC_CHARCILSS))
    ((foreign-syntax LOC_DEF0 "define") char-ci=? (foreign-procedure LOC_STRCIGEQ))
    ((foreign-syntax LOC_DEF0 "define") char-ci>=? (foreign-procedure LOC_CHARCIGEQ))
    ((foreign-syntax LOC_DEF0 "define") char-ci>? (foreign-procedure LOC_CHARCIGTR))

    ((foreign-syntax LOC_DEF0 "define") char-downcase (foreign-procedure LOC_CHARDNCASE))
    ((foreign-syntax LOC_DEF0 "define") char-lower-case? (foreign-procedure LOC_CHARLP))
    ((foreign-syntax LOC_DEF0 "define") char-numeric? (foreign-procedure LOC_CHARNP))
    ((foreign-syntax LOC_DEF0 "define") char-upper-case? (foreign-procedure LOC_CHARUP))
    ((foreign-syntax LOC_DEF0 "define") char-upcase (foreign-procedure LOC_CHARUPCASE))
    ((foreign-syntax LOC_DEF0 "define") char-whitespace? (foreign-procedure LOC_CHARWP))

    ((foreign-syntax LOC_DEF0 "define") string-ci<=? (foreign-procedure LOC_STRCILEQ))
    ((foreign-syntax LOC_DEF0 "define") string-ci<? (foreign-procedure LOC_STRCILSS))
    ((foreign-syntax LOC_DEF0 "define") string-ci=? (foreign-procedure LOC_STRCIEQU))
    ((foreign-syntax LOC_DEF0 "define") string-ci>=? (foreign-procedure LOC_STRCIGEQ))
    ((foreign-syntax LOC_DEF0 "define") string-ci>? (foreign-procedure LOC_STRCIGTR))

  )
)


