;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme message)
  
  (export get-header
	  set-header!

	  get-fields
	  get-field
	  set-fields!

	  get-body
	  set-body!

	  make-message
	  message?

	  string->message
	  message->string
	  )
  (begin


    ))