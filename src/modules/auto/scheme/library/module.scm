;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-initialize (include-string "initialization.c"))
(foreign-finalize (include-string "finalization.c"))

(include "macros.scm")
(include "procedures.scm")

(environment-only (current-environment) 'define-library 'import 'environment-import-sets! 'library-eval)



