;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(foreign-declare (include-string "macros.h"))


(import (auto scheme args)
	(auto scheme args fold)
	(auto scheme base)
	(auto scheme directory)
	(auto scheme lazy)
	(auto scheme write)
	
	;; (scheme cxr)
	(scheme process-context)
	;; (auto scheme string)
	;; (scheme read)
	(auto scheme file)

	;; (auto scheme compile)
	;; (auto scheme interpret)
	)


(define state-path (foreign-string STATE_PATH_STR))
(define lock-file (string-append state-path "/ide/posix/lock.s"))

(cond ((not (file-exists? lock-file))
       (display "autoscheme-mod: AutoScheme is not installed" (current-error-port))(newline (current-error-port))
       (exit 1)))

(write (current-directory))(newline)
(create-directory "tmp_dir/1/2/3" #t)
;; (delete-file lock-file)
;; (write lock-file)(newline)
(exit)


(define program-version (include "../../../version.txt"))

(define display-version
  (lambda ()
    (display (string-append "AutoScheme-mod version " program-version))(newline)
    ))

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
				  (,(option '(#\l "list") #f #f recognized-processor) "List installed modules")
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
       (modules (reverse (cadr result)))

;;        (option-selected? (lambda (name selected-options)
;;        			   (call/cc (lambda (return)
;;        				      (for-each (lambda (selected-option)
;;        						  (if (member name (option-names (car selected-option))) (return selected-option) ))
;;        						selected-options)
;;        				      #f))))

;;        (get-selected-arg (lambda (name)
;;        			   (let ((selected-option (option-selected? name options)))
;;        			     (if selected-option (caddr selected-option) #f))))

;;        (compile-selected (option-selected? "compile" options))
;;        (output-file (get-selected-arg "output-file"))

;;        (compile-module-selected (option-selected? "compile-module" options))
;;        (module-name (get-selected-arg "module-name"))

;;        (load-modules (get-selected-arg "load-modules"))


;;        (interpret-selected (option-selected? "interpret" options))

;;        (module-list (if load-modules (apply append (map (lambda (token)
;; 							  (if (= (string-length token) 0) '()
;; 							      (list token)))
;; 							(string-tokenize load-modules)))

;; 			'()))
       )

(display-version)
(display "options: ")(write options)(newline)
(display "modules: ")(write modules)(newline)

;;   (cond (compile-selected (compile-program source-files module-list output-file))
;;   	(compile-module-selected (compile-module source-files module-name output-file))
;;   	(interpret-selected (apply interpret source-files))
;;   	(else (help-processor))
;;   	)
  
  )
