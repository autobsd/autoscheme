;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(display "inside file1.scm...")(newline)

(write '(include "file2.scm"))(newline)

(write `(`,,(include "file2.scm")))

(newline)