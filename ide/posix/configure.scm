;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (auto scheme base)
	(auto scheme write)
	(auto scheme directory)
	)

(display "configuring build...")(newline)

(define modules '())
(define module-dependencies "")

(define update-dependencies
  (lambda (module)
    (parameterize ((current-directory module))
		  (set! module-dependencies (string-append module-dependencies "\n" module "_dep = \\\n"))
		  (for-each (lambda (file)
			      (set! module-dependencies (string-append module-dependencies "\t$(modules_dir)/" 
								       module "/" file 
								       " \\\n"))
			      )
			    (directory-files))
		  )))

(parameterize ((current-directory "../../src/modules"))

	      (for-each (lambda (module)
			  (update-dependencies module)
			  (set! modules (cons module modules))
			  )
			(directory-files))


	      )

(define module-objects "")




(display module-dependencies)(newline)

(display modules)(newline)
