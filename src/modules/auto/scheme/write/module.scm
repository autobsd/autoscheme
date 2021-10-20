;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme write)

  (export write-simple
	  write
	  display

	  )

  (begin 

    ((foreign-syntax OP_DEF0 "define") display-simple (foreign-procedure OP_DISPLAY))
    ((foreign-syntax OP_DEF0 "define") write-simple (foreign-procedure OP_WRITE))

    ((foreign-syntax OP_DEF0 "define") write write-simple)
    ((foreign-syntax OP_DEF0 "define") display display-simple)

    )
  )

