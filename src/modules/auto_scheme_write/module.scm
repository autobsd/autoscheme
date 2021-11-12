;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme write)

  (export write-simple
	  write
	  display

	  )

  (begin 

    ((foreign-syntax LOC_DEF0 "define") display-simple (foreign-operation LOC_DISPLAY))
    ((foreign-syntax LOC_DEF0 "define") write-simple (foreign-operation LOC_WRITE))

    ((foreign-syntax LOC_DEF0 "define") write write-simple)
    ((foreign-syntax LOC_DEF0 "define") display display-simple)

    )
  )

