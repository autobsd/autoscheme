;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (auto scheme base)
	(auto scheme write)
	)

(display "configuring build...")(newline)



;; (display "current-directory: ")(write (current-directory))(newline)

;; (define modules '())

;; (current-directory "../../src/modules")

;; (let get-modules ((prefix '())
;; 		  )
;;   (let ((files (directory-files)))
;;     (for-each (lambda (file)
;; 		(cond ((directory? file)
;; 		       (display "prefix: ")(write prefix)(newline)
;; 		       (display "found-dir: ")(write file)(newline)
;; 		       (parameterize ((current-directory file))
;; 				     (get-modules (cons file prefix))
;; 				     ))
;; 		      ((string=? file "module.scm")
;; 		       (display "prefix: ")(write prefix)(newline)
;; 		       (display "found-module: ")(write file)(newline)
;; 		       )
;; 		       ))
;; 	      files))


;;   )