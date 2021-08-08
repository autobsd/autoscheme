;; (import (s-markup xml))

;; (display-xml '((html (body (h1 "headline")))))(newline)

;; (import (rename (auto scheme write) (display my-display) (write my-write)))
(import (auto scheme write))
;; (import (except (auto scheme write) display))

(display "test message")(newline)
(write "test message")(newline)

;; (import (auto scheme base))
;; (import (auto scheme write))

;; (display "Regression Tests")(newline)(newline)


;; (display "(+ 2 2): ")(write (+ 2 2))(newline)
;; (display "(- 5 2): ")(write (- 5 2))(newline)
;; (display "(* 3 4): ")(write (* 3 4))(newline)
;; (display "(/ 12 3): ")(write (/ 12 3))(newline)

;; (display "(< 12 3): ")(write (< 12 3))(newline)

;; (display "(<= 12 3): ")(write (<= 12 3))(newline)
;; (display "(<= 3 3): ")(write (<= 3 3))(newline)

;; (display "(= 12 3): ")(write (= 12 3))(newline)
;; (display "(= 3 3): ")(write (= 3 3))(newline)

;; (display "(> 12 3): ")(write (> 12 3))(newline)

;; (display "(>= 12 3): ")(write (>= 12 3))(newline)
;; (display "(>= 3 3): ")(write (>= 3 3))(newline)



