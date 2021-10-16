;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (auto scheme base)
	(auto scheme write)
	(auto scheme args fold)
	(auto scheme args)
	(scheme cxr)
	(auto scheme process context)
	(auto scheme string)
	(auto scheme read)
	(auto scheme file)

	(auto scheme compile)
	(auto scheme interpret)
	)



(define program-version (include "../../version.txt"))

(define display-version
  (lambda ()
    (display (string-append "AutoScheme version " program-version))(newline)
    ))

(define recognized-processor 
  (lambda (option name arg . seeds)
    (let ((options (car seeds))
	  (source-files (cadr seeds)))
      (values (cons (list option name arg) options) source-files)
      )))

(define repl-processor
  (lambda args
    (repl (interaction-environment))
    (exit) ))

(define version-processor
  (lambda args
    (display-version)
    (exit) ))

(define help-processor
  (lambda args
    (display-version)
    (args-usage option-table)
    (exit) ))

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


(define option-table (quasiquote ((,(option '(#\i "interpret") #f #f recognized-processor) "Interpret sources")
				  (,(option '(#\c "compile") #f #f recognized-processor) "Compile sources")
				  (,(option '(#\l "load-modules") #t #f recognized-processor) "Load modules")
				  (,(option '(#\m "compile-module") #f #f recognized-processor) "Compile module")
				  (,(option '(#\n "module-name") #t #f recognized-processor) "Specify compiled module name")
				  (,(option '(#\o "output-file") #t #f recognized-processor) "Specify output file")
				  (,(option '(#\r "repl") #f #f repl-processor) "Enter interactive REPL")
				  (,(option '(#\s "shell") #f #f version-processor) "Enter command line shell")
				  (,(option '(#\V "version") #f #f version-processor) "Display version information")
				  (,(option '(#\h "help") #f #f help-processor) "Show this message")
				  )))





(let* ((seeds (list '() '()))
       (result (call-with-values (lambda () (apply args-fold (append (list (cdr (command-line) )
						    (map car option-table)
						    unrecognized-processor
						    operand-processor
						    )
					      seeds))
				    ) list))


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

       (load-modules (get-selected-arg "load-modules"))


       (interpret-selected (option-selected? "interpret" options))

       (module-list (if load-modules (apply append (map (lambda (token)
							  (if (= (string-length token) 0) '()
							      (list token)))
							(string-tokenize load-modules)))

			'()))
       )


  (cond (compile-selected (compile-program source-files module-list output-file))
  	(compile-module-selected (compile-module source-files module-name output-file))
  	(interpret-selected (apply interpret source-files))
  	(else (help-processor))
  	)
  
  )
