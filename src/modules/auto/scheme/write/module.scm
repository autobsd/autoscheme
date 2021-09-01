;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme write)

  (export display write)

  (import (only (s7) display write object->string))

  (begin
    (define s7_write write)
	 (define write
	   (lambda (obj . args)
	     (if (string? obj)
		 (apply display (cons (object->string obj) args))
		 (apply s7_write (cons obj args)))))
	 )
  )
