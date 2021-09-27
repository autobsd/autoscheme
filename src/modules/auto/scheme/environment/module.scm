;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme environment)
  
  (import (only (auto scheme) 
		global-environment
		current-environment
		make-environment
		environment-defined-symbols
		environment-import!
		environment-delete!
		environment-rename
		environment-prefix
		environment-except
		environment-only
		environment-update!
		environment-ref
		environment-assoc
		environment-undefine!
		environment-define!


		)

	  (only (auto scheme base)
		begin
		)
	  )

  (export global-environment
	  current-environment
	  make-environment
	  environment-defined-symbols
	  environment-import!
	  environment-delete!
	  environment-rename
	  environment-prefix
	  environment-except
	  environment-only
	  environment-update!
	  environment-ref
	  environment-assoc
	  environment-undefine!
	  environment-define!

	  )

  )