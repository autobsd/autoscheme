;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "environment/declarations.h"))
(foreign-define (include-string "environment/definitions.c"))
(foreign-initialize (include-string "environment/initialization.c"))



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


   (include "library/macros.scm")
   (include "library/procedures.scm")
   (include "library/library.scm")
   (include "identifiers.scm")


   (environment-define! (current-environment) (string->symbol "(auto scheme)")
			(apply environment-only (cons (current-environment)
						      (append '() identifiers)
						      )))

   (for-each (lambda (identifier)
	       (environment-delete! parent-environment identifier)
	       )
	     identifiers)

   (environment-import! parent-environment (environment-only (current-environment) 
							     'define-library
							     'import
							     (string->symbol "(auto scheme)")
							     'begin
							     'let
							     ))

   )
 (current-environment))
