;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(environment-define! (current-environment) (string->symbol "(auto scheme environment)")
		     (apply environment-only (cons (current-environment)
						   '( current-environment
						      calling-environment
						      make-environment
						      environment-define!
						      environment-undefine!
						      environment-defined-symbols
						      environment-assoc
						      environment-ref
						      environment-update!
						      environment-import!
						      environment-only
						      environment-except
						      environment-prefix
						      environment-rename))))
