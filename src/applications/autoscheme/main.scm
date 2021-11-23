;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (auto scheme base)
	(auto scheme write)
	(auto scheme args fold)
	(auto scheme args)
	(scheme cxr)
	(scheme process-context)
	(auto scheme string)
	(scheme read)
	(auto scheme file)

	(auto scheme compile)
	(auto scheme interpret)
	)


(define program-version (include "../../../version.txt"))

(define display-version
  (lambda ()
    (display (string-append "AutoScheme version " program-version))(newline)))

(define display-usage
  (lambda args
    (apply display (cons "Usage: autoscheme option ... [argument ...]" args))(apply newline args)
    (apply args-usage (cons option-table args))))   


(define recognized-processor 
  (lambda (option name arg . seeds)
    (let ((options (car seeds))
	  (arguments (cadr seeds)))
      (values (cons (list option name arg) options) arguments)
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
    (display-usage)
    (exit) ))

(define unrecognized-processor 
  (lambda (option name arg . seeds)
    (let ((options (car seeds))
	  (arguments (cadr seeds))
	  (name-string (if (char? name) 
			   (string-append "-" (string name))
			   (string-append "--" name))))
      (cond ((assoc interpret-option options) 
	     (values options 
		     (if (char? name) 
			 (cons arg (cons name-string arguments))
			 (cons (string-append name-string "=" arg ) arguments))))

	    (else (display (string-append "autoscheme: unrecognized option " name-string) (current-error-port))(newline (current-error-port))
		  (display-usage (current-error-port))
		  (exit 1) )))))

(define operand-processor 
  (lambda (operand . seeds)
    (let ((options (car seeds))
	  (arguments (cadr seeds)))

      (values options (cons operand arguments)))))

(define interpret-option (option '(#\i "interpret") #t #f recognized-processor))

(define option-table `((,interpret-option "Interpret program files" "FILES")
		       (,(option '(#\c "compile") #t #f recognized-processor) "Compile program files" "FILES")
		       (,(option '(#\l "load-modules") #t #f recognized-processor) "Load module files" "FILES")
		       (,(option '(#\m "compile-module") #t #f recognized-processor) "Compile module files" "FILES")
		       (,(option '(#\n "module-name") #t #f recognized-processor) "Specify module name" "NAME")
		       (,(option '(#\o "output-file") #t #f recognized-processor) "Specify output file" "FILE")
		       ;; (,(option '(#\r "repl") #f #f repl-processor) "Enter interactive REPL")
		       ;; (,(option '(#\s "shell") #f #f version-processor) "Enter command line shell")
		       (,(option '(#\V "version") #f #f version-processor) "Display version information")
		       (,(option '(#\h "help") #f #f help-processor) "Show this message")
		       ))



(let* ((seeds (list '() '()))
       (result (call-with-values (lambda () (apply args-fold (append (list (cdr (command-line) )
						    (map car option-table)
						    unrecognized-processor
						    operand-processor
						    )
					      seeds))
				    ) list))


       (options (car result))
       (arguments (reverse (cadr result)))

       (option-selected? (lambda (name selected-options)
       			   (call/cc (lambda (return)
       				      (for-each (lambda (selected-option)
       						  (if (member name (option-names (car selected-option))) (return selected-option) ))
       						selected-options)
       				      #f))))

       (get-selected-arg (lambda (name)
       			   (let ((selected-option (option-selected? name options)))
       			     (if selected-option (caddr selected-option) #f))))

       (arg->list (lambda (arg)
		    (if arg (apply append (map (lambda (token)
						 (if (= (string-length token) 0) '()
						     (list token)))
					       (string-tokenize arg)))
       			'())))
       )


  (cond ((option-selected? "interpret" options) (interpret (arg->list (get-selected-arg "interpret")) arguments))

	((option-selected? "compile" options) (compile-program (arg->list (get-selected-arg "compile")) 
							       (arg->list (get-selected-arg "load-modules")) 
							       (get-selected-arg "output-file")))

	((option-selected? "compile-module" options) (compile-module (arg->list (get-selected-arg "compile-module")) 
								     (get-selected-arg "module-name")
								     (get-selected-arg "output-file")))

	(else (help-processor))
	)
  )
