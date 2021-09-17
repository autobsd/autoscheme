;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

((lambda (parent-environment)
   (environment-import! (current-environment) parent-environment)

   (define object->string
     (lambda (object)
       (let ((string-port (open-output-string)))
	 (write object string-port)
	 (let ((output-string (get-output-string string-port)))
	   (close-output-port string-port)
	   output-string))
       ))


   (display "inside (auto scheme library)")(newline)
   (include "macros.scm")
   (include "procedures.scm")
   (include "library.scm")




   (environment-import! parent-environment (environment-only (current-environment) 
							     'make-library
							     'environment-import-sets!
							     'define-library
							     'import
							     (string->symbol "(auto scheme library)")
							     ))
   )


 (current-environment)
 )