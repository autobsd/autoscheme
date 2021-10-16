;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-initialize (include-string "initialization.c"))
(foreign-finalize (include-string "finalization.c"))

(define eval (foreign-procedure OP_PEVAL))
(define write-simple (foreign-procedure OP_WRITE))

(include "macros.scm")
(include "procedures.scm")

(environment-import! (foreign-pointer module_environment) (environment-only (current-environment) 
									    ((foreign-procedure OP_STR2SYM) "define-library")
									    ((foreign-procedure OP_STR2SYM) "import")
									    ))

(environment-delete! (foreign-pointer module_environment) ((foreign-procedure OP_STR2SYM) "(scheme)"))
