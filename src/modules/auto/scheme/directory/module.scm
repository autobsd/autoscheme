;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-define (include-string "definitions.c"))


(define-library (auto scheme directory)

  (export current-directory)


  (begin 
    
    ((foreign-syntax LOC_DEF0 "define") current-directory (foreign-function ff_current_directory))

  ))