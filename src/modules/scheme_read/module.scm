;;  This file is part of the 'AutoScheme' project.
;;  Copyright (c) 2026 AutoBSD Contributors.
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (scheme read)

  (export read)

  (begin

    ((foreign-syntax LOC_DEF0 "define") read (foreign-operation LOC_READ))
    
    ))
