;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme file)

  (export call-with-input-file
	  call-with-output-file
	  
	  open-input-file
	  open-output-file 

	  with-input-from-file
	  with-input-to-file
	  )

  (begin

    ((foreign-syntax OP_DEF0 "define") call-with-input-file (foreign-procedure OP_CALL_INFILE0))
    ((foreign-syntax OP_DEF0 "define") call-with-output-file (foreign-procedure OP_CALL_OUTFILE0))

    ((foreign-syntax OP_DEF0 "define") open-input-file (foreign-procedure OP_OPEN_INFILE))
    ((foreign-syntax OP_DEF0 "define") open-output-file (foreign-procedure OP_OPEN_OUTFILE))

    ((foreign-syntax OP_DEF0 "define") with-input-from-file (foreign-procedure OP_WITH_INFILE0))
    ((foreign-syntax OP_DEF0 "define") with-input-to-file (foreign-procedure OP_WITH_OUTFILE0))
    )
  )
