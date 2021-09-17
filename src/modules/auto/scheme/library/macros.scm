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
    (quasiquote (environment-define! (current-environment) ',name-symbol (make-library (_unquote-splicing quoted-declarations))))
    ))


(define-macro (import . sets)

  (_quasiquote (environment-import-sets! (current-environment) (_unquote-splicing (map (lambda (set) (_quasiquote (quote ,set))) sets))))

  )


