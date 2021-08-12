(display "inside file1.scm...")(newline)
(write `(`,,(include "file2.scm")))
(newline)