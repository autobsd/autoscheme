;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme memory)

  (export (rename gc collect-garbage)
	  gc-verbose
	  )

  (begin

    ((foreign-syntax OP_DEF0 "define") gc (foreign-procedure OP_GC))
    ((foreign-syntax OP_DEF0 "define") gc-verbose (foreign-procedure OP_GCVERB))


  )
)


