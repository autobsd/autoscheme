;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare "foreign_function ff_path_make_absolute;\n"
		 "foreign_function ff_path_directory;\n")


(define-library (scheme load)

  (import (auto scheme base)
	  (scheme eval)
	  (scheme file)
	  (scheme read)
	  )

  (export load)

  (begin
    (define current-source (foreign-operation LOC_CURR_SOURCE))

    (define path-make-absolute (foreign-function ff_path_make_absolute))
    (define path-directory (foreign-function ff_path_directory))
	 

     (define load
       (lambda (filename . rest)

	 (define load-environment (if (null? rest) 
				      (interaction-environment)
				      (car rest)))

	 (parameterize ((current-source (path-make-absolute filename)))

		       (with-input-from-file filename
			 (lambda ()
			   (if (char=? (peek-char) #\#) (read-line))
			   (let load-expression ((expression (read)))
			     (cond ((not (eof-object? expression))
				    (eval expression load-environment)
				    (load-expression (read))))
			     )

			   )))
	 ))))