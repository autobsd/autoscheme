;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-define (include-string "definitions.c"))


(define-library (auto scheme directory)

  (export create-directory
	  current-directory
	  directory-files 
	  directory?
	  )


  (begin 
    
    ((foreign-syntax LOC_DEF0 "define") create-directory (foreign-function ff_create_directory))
    ((foreign-syntax LOC_DEF0 "define") current-directory (foreign-function ff_current_directory))
    ((foreign-syntax LOC_DEF0 "define") directory-files (foreign-function ff_directory_files))
    ((foreign-syntax LOC_DEF0 "define") directory? (foreign-function ff_directory_p))



  ))