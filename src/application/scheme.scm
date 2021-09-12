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
  (write (environment-defined-symbols (current-environment)))(newline)
  ;; (quit)



  ;; (display  (car (current-environment)))(newline)
  ;; ;; (display "myvar4: ")(write myvar4)(newline)

  ;; (newline)
  ;; (write (environment-defined-symbols (current-environment)))
  (newline)
  

  )
  ;; (write (environment-defined-symbols (current-environment)))(newline)

(display "myvar1: ")(write myvar1)(newline)



