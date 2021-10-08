;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (auto scheme write)
	(auto scheme base))

(display "hello world\n")
(write `(3 2 1 ,mistake))
