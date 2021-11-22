;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme args)
  
  (import (auto scheme base)
	  (auto scheme write)
	  (auto scheme args fold)
	  (auto scheme string)
	  (scheme cxr)
	  )

  (export args-usage)

  (begin

    (define args-usage
      (lambda (opt-tab)

	(let* ((max-short 0)
	       (max-long 0)
	       ;; (max-description 0)

	       (opt-strings (map (lambda (row)
				   (let* ((opt (car row))
					  (names (option-names opt))
					  
					  (short-names (apply append (map (lambda (name) (if (char? name) (list name) '())) names)))
					  (long-names (apply append (map (lambda (name) (if (string? name) (list name) '())) names)))

					  (name-delimiter (if (or (null? short-names)(null? long-names)) "  " ", "))

					  (short-string (string-join (map (lambda (short-name) (string-append "-" (string short-name))) short-names) ", " ))
					  (long-string (string-join (map (lambda (long-name) (string-append "--" long-name)) long-names) ", " ))
					  (description (cadr row))

					  (short-length (string-length short-string))
					  (long-length (string-length long-string))
					  ;; (description-length (string-length description))
					  )

				     (cond ((> short-length max-short) (set! max-short short-length))
					   ((> long-length max-long) (set! max-long long-length))
					   ;; ((> description-length max-description) (set! max-description description-length))
					   )

				     (list short-string name-delimiter long-string description)

				     ))
				 opt-tab)
			    )
	       )

	  (map (lambda (row)
		 (let* ((short-string (car row))
			(delimiter (cadr row))
			(long-string (caddr row))
			(description (cadddr row))
			(padding-short (make-string (- max-short (string-length short-string)) #\space))
			(padding-long (make-string (- max-long (string-length long-string)) #\space))
			
			)
		   (display (string-append " " padding-short short-string delimiter long-string padding-long "  " description))(newline)

		   ))
	       opt-strings)

	  )
	))
    ))