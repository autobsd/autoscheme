;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "declarations.h"))
(foreign-define (include-string "definitions.c"))


(define-library (auto scheme path)

  (export path-directory
	  path-absolute?
	  path-make-absolute)

  (begin ((foreign-syntax LOC_DEF0 "define") path-absolute? (foreign-function ff_path_absolute_p))
	 ((foreign-syntax LOC_DEF0 "define") path-make-absolute (foreign-function ff_path_make_absolute))
	 ((foreign-syntax LOC_DEF0 "define") path-directory (foreign-function ff_path_directory))))
