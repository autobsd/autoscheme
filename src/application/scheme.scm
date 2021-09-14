;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(display "inside AutoScheme application...\n")




(define myvar1 1)

(let ()

  (display "myvar1: ")(write myvar1)(newline)
  (define myvar1 11)
  (display "myvar1: ")(write myvar1)(newline)

  ;; (environment-undefine! (current-environment) 'myvar1)
  ;; (write (environment-defined-symbols (current-environment)))(newline)
  ;; (quit)

  (write (environment-assoc (current-environment) 'myvar1)) (newline)

  ;; (display  (car (current-environment)))(newline)
  ;; ;; (display "myvar4: ")(write myvar4)(newline)

  ;; (newline)
  ;; (write (environment-defined-symbols (current-environment)))
  (newline)
  

  )
  ;; (write (environment-defined-symbols (current-environment)))(newline)
(environment-update! (current-environment) 'myvar1 2)

(display "myvar1: ")(write myvar1)(newline)
(write (environment-ref (current-environment) 'myvar1)) (newline)

(define env (apply make-environment '((x . 3)(y . 4)(z . 5))))
(define env2 (apply make-environment '((a . 1)(b . 2)(c . 3))))
(define env3 (apply make-environment '((d . 1)(e . 2)(f . 3))))

(environment-import! (current-environment) (environment-rename env '(z . zzz)) (environment-only env2 'a 'b) (environment-except env3 'd 'e) (environment-prefix env2 'pre-))
(display "defined symbols: ")(write (environment-defined-symbols (current-environment)))(newline)
(newline)
(display "f: ")(write f)(newline)
(display "pre-b: ")(write pre-b)(newline)
