;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme port)

  (export open-input-output-file
	  open-input-output-string
	  )

  (begin

    ((foreign-syntax LOC_DEF0 "define") open-input-output-file (foreign-operation LOC_OPEN_INOUTFILE))
    ((foreign-syntax LOC_DEF0 "define") open-input-output-string (foreign-operation LOC_OPEN_INOUTSTRING))

    )
  )
