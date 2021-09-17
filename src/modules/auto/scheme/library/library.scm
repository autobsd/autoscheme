;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(environment-define! (current-environment) (string->symbol "(auto scheme library)")
		     (apply environment-only (cons (current-environment)
						   '( make-library
						      environment-import-sets!
						      define-library
						      import))))

