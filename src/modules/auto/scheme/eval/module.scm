;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme eval)

  (import (only (auto scheme) 
  		begin
  		define-macro
  		lambda
		eval
		quasiquote
		quote
		map

		append
		cons
  		))

  (export environment eval)
  (begin

    (define-macro (environment . sets) (_quasiquote (environment-import-sets! (make-environment) (_unquote-splicing (map (lambda (set) (_quasiquote (quote ,set))) sets)))))

    ))


