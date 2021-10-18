;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (auto scheme write)
	(auto scheme base) 
	(auto scheme directory)
	)

(display "testing return values:\n")
(write (define x (+ 2 2)))(newline)
(write (current-directory)) (newline)