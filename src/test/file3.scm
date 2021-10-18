;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (auto scheme write)
	(auto scheme base) 
	(auto scheme directory)
	(scheme process-context)
	)

(display "testing:\n")

(write (current-directory)) (newline)
(write (command-line))(newline)
(write (get-environment-variables))(newline)
(write (get-environment-variable "HOME"))(newline)
