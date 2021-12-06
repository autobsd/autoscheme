;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-define (include-string "definitions.c"))

(define-library (auto scheme file)

  ;; (import (auto scheme base)
  ;; 	  (only (scheme file) 	  
  ;; 		delete-file
  ;; 		file-exists?)
  ;; 	  )


  (export open-binary-input-output-file
	  open-input-output-file
	  rename-file
	  )

  (begin

    ((foreign-syntax LOC_DEF0 "define") open-binary-input-output-file (foreign-operation LOC_OPEN_BINOUTFILE))
    ((foreign-syntax LOC_DEF0 "define") open-input-output-file (foreign-operation LOC_OPEN_INOUTFILE))


    ((foreign-syntax LOC_DEF0 "define") rename-file (foreign-function ff_rename_file))


    ;; (define rename-file
    ;;   (lambda (old-name new-name . rest)
    ;; 	(let ((replace (and (pair? rest) (car rest))))
    ;; 	  (cond ((not (file-exists? old-name)) (error "File error - no such file or directory" old-name))
    ;; 		((not (file-exists? new-name)))
    ;; 		(replace (delete-file new-file #t #t))
    ;; 		(else (error "File error - file exists" new-name)))
	  
    ;; 	  (_rename-file old-name
    )
  )
