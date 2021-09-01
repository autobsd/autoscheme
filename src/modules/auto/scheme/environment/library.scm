;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme environment)
  (export make-environment 
	  current-environment
	  environment-ref
	  environment-remove! 
	  environment-defined
	  environment-defined? 
	  environment-update! 
	  environment-import! 
	  environment-only
	  environment-except
	  environment-prefix	  
	  environment-rename

	  define-library 
	  import)
  )

