;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme file)
  (import (only (auto scheme) open-input-file open-output-file with-input-from-file
		begin
		))

  (export open-input-file
	  open-output-file 
	  with-input-from-file)


  )