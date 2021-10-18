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

    ((foreign-syntax OP_DEF0 "define") char-alphabetic? (foreign-procedure OP_CHARAP))

    ((foreign-syntax OP_DEF0 "define") char-ci<=? (foreign-procedure OP_CHARCILEQ))
    ((foreign-syntax OP_DEF0 "define") char-ci<? (foreign-procedure OP_CHARCILSS))
    ((foreign-syntax OP_DEF0 "define") char-ci=? (foreign-procedure OP_STRCIGEQ))
    ((foreign-syntax OP_DEF0 "define") char-ci>=? (foreign-procedure OP_CHARCIGEQ))
    ((foreign-syntax OP_DEF0 "define") char-ci>? (foreign-procedure OP_CHARCIGTR))

    ((foreign-syntax OP_DEF0 "define") char-downcase (foreign-procedure OP_CHARDNCASE))
    ((foreign-syntax OP_DEF0 "define") char-lower-case? (foreign-procedure OP_CHARLP))
    ((foreign-syntax OP_DEF0 "define") char-numeric? (foreign-procedure OP_CHARNP))
    ((foreign-syntax OP_DEF0 "define") char-upper-case? (foreign-procedure OP_CHARUP))
    ((foreign-syntax OP_DEF0 "define") char-upcase (foreign-procedure OP_CHARUPCASE))
    ((foreign-syntax OP_DEF0 "define") char-whitespace? (foreign-procedure OP_CHARWP))

    ((foreign-syntax OP_DEF0 "define") string-ci<=? (foreign-procedure OP_STRCILEQ))
    ((foreign-syntax OP_DEF0 "define") string-ci<? (foreign-procedure OP_STRCILSS))
    ((foreign-syntax OP_DEF0 "define") string-ci=? (foreign-procedure OP_STRCIEQU))
    ((foreign-syntax OP_DEF0 "define") string-ci>=? (foreign-procedure OP_STRCIGEQ))
    ((foreign-syntax OP_DEF0 "define") string-ci>? (foreign-procedure OP_STRCIGTR))

  )
)


