;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2022 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define uninstall-modules
  (lambda (modules)
    (let ((modified #f))
      (for-each (lambda (module)

		  (cond ((not (member module (list-installed-modules)))
			 (display "Module is not installed: ")(display module)(newline))

			(else (set! modified #t)

			      (display "uninstalling: ")(display module)(display "...")(newline)
			      (let ((modules-src-dir (path-make-absolute "src/modules/" state-path)))

				(parameterize ((current-directory modules-src-dir))
					      (delete-file module #t #t) ))

			      (display "uninstalling: ")(display module)(display "...DONE")(newline)
			      )))
		modules)
      
      (cond (modified (rebuild-package) (reinstall-package)))
      )))