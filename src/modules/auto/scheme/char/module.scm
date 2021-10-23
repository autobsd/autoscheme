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

    ((foreign-syntax LOC_DEF0 "define") char-alphabetic? (foreign-operation LOC_CHARAP))

    ((foreign-syntax LOC_DEF0 "define") char-ci<=? (foreign-operation LOC_CHARCILEQ))
    ((foreign-syntax LOC_DEF0 "define") char-ci<? (foreign-operation LOC_CHARCILSS))
    ((foreign-syntax LOC_DEF0 "define") char-ci=? (foreign-operation LOC_STRCIGEQ))
    ((foreign-syntax LOC_DEF0 "define") char-ci>=? (foreign-operation LOC_CHARCIGEQ))
    ((foreign-syntax LOC_DEF0 "define") char-ci>? (foreign-operation LOC_CHARCIGTR))

    ((foreign-syntax LOC_DEF0 "define") char-downcase (foreign-operation LOC_CHARDNCASE))
    ((foreign-syntax LOC_DEF0 "define") char-lower-case? (foreign-operation LOC_CHARLP))
    ((foreign-syntax LOC_DEF0 "define") char-numeric? (foreign-operation LOC_CHARNP))
    ((foreign-syntax LOC_DEF0 "define") char-upper-case? (foreign-operation LOC_CHARUP))
    ((foreign-syntax LOC_DEF0 "define") char-upcase (foreign-operation LOC_CHARUPCASE))
    ((foreign-syntax LOC_DEF0 "define") char-whitespace? (foreign-operation LOC_CHARWP))

    ((foreign-syntax LOC_DEF0 "define") string-ci<=? (foreign-operation LOC_STRCILEQ))
    ((foreign-syntax LOC_DEF0 "define") string-ci<? (foreign-operation LOC_STRCILSS))
    ((foreign-syntax LOC_DEF0 "define") string-ci=? (foreign-operation LOC_STRCIEQU))
    ((foreign-syntax LOC_DEF0 "define") string-ci>=? (foreign-operation LOC_STRCIGEQ))
    ((foreign-syntax LOC_DEF0 "define") string-ci>? (foreign-operation LOC_STRCIGTR))

  )
)


