(import (scheme base))
(import (scheme write))

(display "Regression Tests")(newline)(newline)


(display "(+ 2 2): ")(write (+ 2 2))(newline)
(display "(- 5 2): ")(write (- 5 2))(newline)
(display "(* 3 4): ")(write (* 3 4))(newline)
(display "(/ 12 3): ")(write (/ 12 3))(newline)

(display "(< 12 3): ")(write (< 12 3))(newline)

(display "(<= 12 3): ")(write (<= 12 3))(newline)
(display "(<= 3 3): ")(write (<= 3 3))(newline)

(display "(= 12 3): ")(write (= 12 3))(newline)
(display "(= 3 3): ")(write (= 3 3))(newline)

(display "(> 12 3): ")(write (> 12 3))(newline)

(display "(>= 12 3): ")(write (>= 12 3))(newline)
(display "(>= 3 3): ")(write (>= 3 3))(newline)



