;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (scheme read)

  (export read)

  (begin

    ((foreign-syntax OP_DEF0 "define") read (foreign-procedure OP_READ))
    
    ))
