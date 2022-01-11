;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2022 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "macros.h"))


(import (auto scheme args)
	(auto scheme args fold)
	(auto scheme base)
	(auto scheme directory)
	(auto scheme file)
	(auto scheme lazy)
	(auto scheme path)
	(auto scheme process)
	(auto scheme write)
	(scheme file)
	(scheme process-context)
	(scheme read)
	)


(define install-path (foreign-string INSTALL_PATH_STR))
(define state-path (foreign-string STATE_PATH_STR))

(define prime-path (path-make-absolute "libexec/autoscheme-prime" install-path))
(define lock-path (path-make-absolute "pkg/posix/lock.s" state-path))

(define list-installed-modules
  (lambda ()
    (guard (condition
    	    ((file-error? condition) (display "AutoScheme is not installed" (current-error-port))(newline (current-error-port))(exit 1))
    	    (else (raise condition)))

	   (with-input-from-file lock-path read) )))


(include "install.scm")
(include "uninstall.scm")



(define display-usage
  (lambda ()
    (display "Usage: autoscheme-mod options... [modules...]")(newline)
    (args-usage option-table)))    


(define recognized-processor 
  (lambda (option name arg . seeds)
    (let ((options (car seeds))
	  (modules (cadr seeds)))
      (values (cons (list option name arg) options) modules)
      )))

(define help-option-processor
  (lambda args
    (display-version)
    (display-usage)
    (exit) ))

(define list-option-processor
  (lambda args
    (map (lambda (module)
	   (display module)(newline))
	 (list-installed-modules))
    (exit)))

(define unrecognized-processor 
  (lambda (option name arg . seeds)
    (let ((name-string (if (char? name) 
			   (string-append "-" (string name))
			   (string-append "--" name)))
	  )

      (display (string-append "autoscheme-mod: unrecognized option " name-string))(newline)
      (display-usage)
      (exit 1) )))

(define operand-processor 
  (lambda (operand . seeds)
    (let ((options (car seeds))
	  (modules (cadr seeds)))
      (values options (cons operand modules)))))


(define option-table (quasiquote ((,(option '(#\i "install") #f #f recognized-processor) "Install modules")
				  (,(option '(#\u "uninstall") #f #f recognized-processor) "Uninstall modules")
				  (,(option '("update") #f #f recognized-processor) "Update modules")
				  (,(option '(#\l "list") #f #f list-option-processor) "List installed modules")
				  (,(option '(#\h "help") #f #f help-option-processor) "Show this message")
				  )))



(let* ((seeds (list '() '()))
       (result (call-with-values (lambda () (apply args-fold (append (list (cdr (command-line) )
								      (map car option-table)
								      unrecognized-processor
								      operand-processor
								      )
								seeds))
				    ) list))


       (selected-options (car result))
       (modules (reverse (cadr result)))

       (option-selected? (lambda (name)
       			   (call/cc (lambda (return)
       				      (for-each (lambda (selected-option)
       						  (if (member name (option-names (car selected-option))) (return selected-option) ))
       						selected-options)
       				      #f))))
       )


  (cond ((option-selected? "install") (install-modules modules))
  	((option-selected? "uninstall") (uninstall-module (car modules)))
  	((option-selected? "update") (update-module (car modules)))
  	)
  
  )
