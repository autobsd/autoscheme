;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2022 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define uninstall-module
  (lambda (module)
    (cond ((not (member module (list-installed-modules)))
	   (display "Module is not installed: ")(display module)(newline)
	   (exit)))

    (display "uninstalling: ")(display module)(display "...")(newline)
    (let ((modules-src-dir (path-make-absolute "src/modules/" state-path)))

      (parameterize ((current-directory modules-src-dir))
		    (delete-file module #t #t) ))

    (build-project)
    (install-project)
    (display "uninstalling: ")(display module)(display "...DONE")(newline)
    ))
  