;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (auto scheme base)
	(auto scheme write)

	)


(define program-version (include "../../version.txt"))

(define display-version
  (lambda ()
    (display (string-append "AutoScheme version " program-version))(newline)
    ))


(display-version)