;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-define (include-string "definitions.c"))

(define-library (auto scheme file)


  (export open-binary-input-output-file
	  open-input-output-file
	  rename-file
	  )

  (begin

    ((foreign-syntax LOC_DEF0 "define") copy-file (foreign-function ff_copy_file))

    ((foreign-syntax LOC_DEF0 "define") open-binary-input-output-file (foreign-operation LOC_OPEN_BINOUTFILE))
    ((foreign-syntax LOC_DEF0 "define") open-input-output-file (foreign-operation LOC_OPEN_INOUTFILE))

    ((foreign-syntax LOC_DEF0 "define") rename-file (foreign-function ff_rename_file))

    )
  )
