;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-macro (define-library name . declarations)
  (let ((name-symbol (string->symbol (object->string name)))
	(quoted-declarations (map (lambda (declaration)
				    (_quasiquote ',declaration)
				    )
				  declarations))
	)

    (apply environment-define! (cons (expansion-environment) (cons name-symbol (list (eval-library declarations (expansion-environment))))))

    ))


(define-macro (import . sets)

  (apply environment-import-sets! (cons (expansion-environment) (cons (expansion-environment) sets)))

  )


