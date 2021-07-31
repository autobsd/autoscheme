(import (scheme load))
(import (scheme read))
(import (auto scheme write))
(import (auto scheme base))
(import (auto srfi 13))
(import (srfi 37))


(define display-usage
  (lambda (opt-tab)
    (display "Usage: autoscheme [options...] [sources...]")(newline)

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

(define display-version
  (lambda ()
    (display (string-append "AutoScheme version " (*version*)))(newline)
    ))

(define help-processor
  (lambda args
    (display-version)
    (display-usage option-table)
    (exit) ))

(define version-processor
  (lambda args
    (display-version)
    (exit) ))

(define recognized-processor 
  (lambda (option name arg . seeds)
    (let ((options (car seeds))
	  (source-files (cadr seeds)))

      (values (cons (list option name arg) options) source-files)

      )))




(define unrecognized-processor 
  (lambda (option name arg . seeds)
    (let ((name-string (if (char? name) 
			   (string-append "-" (string name))
			   (string-append "--" name)))
	  )

      (display (string-append "autoscheme: unrecognized option " name-string))(newline)
      (display-usage option-table)
      (exit 1) )))


(define operand-processor 
  (lambda (operand . seeds)
    (let ((options (car seeds))
	  (source-files (cadr seeds)))

      (values options (cons operand source-files)))))


(define option-table `((,(option '(#\i "interpret") #f #f recognized-processor) "Interpret sources with linked modules")
		       (,(option '(#\c "compile") #f #f recognized-processor) "Compile sources with linked modules")
		       (,(option '(#\l "link-modules") #f #f recognized-processor) "Link modules")
		       (,(option '(#\m "compile-module") #f #f recognized-processor) "Compile module")
		       (,(option '(#\n "module-name") #f #f recognized-processor) "Specify compiled module name")
		       (,(option '(#\o "output-file") #t #f recognized-processor) "Specify output file")
		       (,(option '(#\r "repl") #f #f version-processor) "Enter interactive REPL")
		       (,(option '(#\s "shell") #f #f version-processor) "Enter command line shell")
		       (,(option '(#\V "version") #f #f version-processor) "Display version information")
		       (,(option '(#\h "help") #f #f help-processor) "Show this message")
		       ))



(let* ((seeds (list '() '()))
       (result (list (args-fold (cdr (command-line) )
				(map car option-table)
				unrecognized-processor
				operand-processor
				(apply values seeds)
				)))
       (options (car result))
       (source-files (reverse (cadr result)))

       (option-selected? (lambda (name selected-options)
			   (call/cc (lambda (return)
				      (for-each (lambda (selected-option)
						  (if (member name (option-names (car selected-option))) (return selected-option) ))
						selected-options)
				      #f))))

       (compile-selected (option-selected? "compile" options))
       (output-file-selected (option-selected? "output-file" options))
       (output-file (if output-file-selected (caddr output-file-selected) #f))

       (interpret-selected (option-selected? "interpret" options))
       
       )

  (cond (compile-selected (compile-program source-files output-file))
	(interpret-selected (interpret-program source-files))
	)
  
  )



