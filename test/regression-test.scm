(display "Regression Tests")(newline)(newline)



(define-library (test lib) 
  (export a b c)
  (export x y z)
  (begin
    (define a 1)(define b 2)(define c 3)
    (define x 3)(define y 4)(define z 5)
    ))


(import (test lib))

(display "a: ")(write a)(newline)
(display "b: ")(write b)(newline)
(display "c: ")(write c)(newline)
(display "x: ")(write x)(newline)
(display "y: ")(write y)(newline)
(display "z: ")(write z)(newline)
