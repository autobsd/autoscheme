;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme file)

  (export call-with-input-file
	  call-with-output-file
	  
	  open-input-file
	  open-output-file 

	  with-input-from-file
	  with-output-to-file
	  )

  (begin

    ((foreign-syntax LOC_DEF0 "define") call-with-input-file (foreign-procedure LOC_CALL_INFILE0))
    ((foreign-syntax LOC_DEF0 "define") call-with-output-file (foreign-procedure LOC_CALL_OUTFILE0))

    ((foreign-syntax LOC_DEF0 "define") open-input-file (foreign-procedure LOC_OPEN_INFILE))
    ((foreign-syntax LOC_DEF0 "define") open-output-file (foreign-procedure LOC_OPEN_OUTFILE))

    ((foreign-syntax LOC_DEF0 "define") with-input-from-file (foreign-procedure LOC_WITH_INFILE0))
    ((foreign-syntax LOC_DEF0 "define") with-output-to-file (foreign-procedure LOC_WITH_OUTFILE0))
    )
  )
