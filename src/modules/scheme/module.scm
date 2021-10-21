;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-initialize (include-string "initialization.c"))
(foreign-finalize (include-string "finalization.c"))

(define eval (foreign-procedure LOC_PEVAL))
(define write-simple (foreign-procedure LOC_WRITE))

(include "macros.scm")
(include "procedures.scm")

(environment-import! (foreign-pointer module_environment) (environment-only (current-environment) 
									    ((foreign-procedure LOC_STR2SYM) "define-library")
									    ((foreign-procedure LOC_STR2SYM) "import")
									    ))

(environment-delete! (foreign-pointer module_environment) ((foreign-procedure LOC_STR2SYM) "(scheme)"))
