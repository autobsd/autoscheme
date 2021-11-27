;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-define (include-string "definitions.c"))

(define-library (auto scheme file)

  (export call-with-input-file
	  call-with-output-file
	  
	  file-exists?

	  open-input-file
	  open-output-file 

	  with-input-from-file
	  with-output-to-file
	  )

  (begin

    ((foreign-syntax LOC_DEF0 "define") call-with-input-file (foreign-operation LOC_CALL_INFILE0))
    ((foreign-syntax LOC_DEF0 "define") call-with-output-file (foreign-operation LOC_CALL_OUTFILE0))

    ((foreign-syntax LOC_DEF0 "define") file-exists? (foreign-function ff_file_exists_p))

    ((foreign-syntax LOC_DEF0 "define") open-input-file (foreign-operation LOC_OPEN_INFILE))
    ((foreign-syntax LOC_DEF0 "define") open-output-file (foreign-operation LOC_OPEN_OUTFILE))

    ((foreign-syntax LOC_DEF0 "define") with-input-from-file (foreign-operation LOC_WITH_INFILE0))
    ((foreign-syntax LOC_DEF0 "define") with-output-to-file (foreign-operation LOC_WITH_OUTFILE0))
    )
  )
