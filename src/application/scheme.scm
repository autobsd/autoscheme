;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (auto scheme base)
	(auto scheme write)
	(auto scheme args fold)
	(auto scheme args)

	(auto scheme process context)
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


(help-processor)