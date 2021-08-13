(import (auto scheme load)
	(auto scheme read)
	(auto scheme repl)
	(auto scheme cxr)
	(auto scheme write)
	(auto scheme base)
	(auto scheme eval)
	(auto scheme string)
	(auto scheme args fold)
	(auto scheme args)
	(auto scheme compile)
	(auto scheme interpret)
	)



(define program-version (include "../version.txt"))

(define display-version
  (lambda ()
    (display (string-append "AutoScheme version " program-version))(newline)
    ))

(define help-processor
  (lambda args
    (display-version)
    (args-usage option-table)
    (exit) ))

(define version-processor
  (lambda args
    (display-version)
    (exit) ))



(define repl-processor
  (lambda args
    (repl (interaction-environment))
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
      (args-usage option-table)
      (exit 1) )))


(define operand-processor 
  (lambda (operand . seeds)
    (let ((options (car seeds))
	  (source-files (cadr seeds)))

      (values options (cons operand source-files)))))


(define option-table `((,(option '(#\i "interpret") #f #f recognized-processor) "Interpret sources")
		       (,(option '(#\c "compile") #f #f recognized-processor) "Compile sources")
		       (,(option '(#\l "link-modules") #t #f recognized-processor) "Link modules")
		       (,(option '(#\m "compile-module") #f #f recognized-processor) "Compile module")
		       (,(option '(#\n "module-name") #t #f recognized-processor) "Specify compiled module name")
		       (,(option '(#\o "output-file") #t #f recognized-processor) "Specify output file")
		       (,(option '(#\r "repl") #f #f repl-processor) "Enter interactive REPL")
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

       (get-selected-arg (lambda (name)
			   (let ((selected-option (option-selected? name options)))
			     (if selected-option (caddr selected-option) #f))))

       (compile-selected (option-selected? "compile" options))
       (output-file (get-selected-arg "output-file"))

       (compile-module-selected (option-selected? "compile-module" options))
       (module-name (get-selected-arg "module-name"))

       (link-modules (get-selected-arg "link-modules"))


       (interpret-selected (option-selected? "interpret" options))


       (module-list (if link-modules (string-tokenize link-modules) '()))
       )


  (cond (compile-selected (compile-program source-files module-list output-file))
	(compile-module-selected (compile-module source-files module-name output-file))
	(interpret-selected (interpret-program source-files))
	(else (help-processor))
	)
  
  )



