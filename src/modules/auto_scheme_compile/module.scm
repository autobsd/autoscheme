;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme compile)

  (export compile-program
	  compile-module
	  module-loader-name
	  )

  (import (auto scheme path)
	  (auto scheme base)
	  (auto scheme write)
	  (scheme cxr)
	  (scheme file)
	  (scheme read)
	  )

  (begin


    (define object->string
      (lambda (object)
	(let ((string-port (open-output-string)))
	  (write-simple object string-port)
	  (let ((output-string (get-output-string string-port)))
	    (close-output-port string-port)
	    output-string))
	))

    (define read-list
      (lambda args
	(reverse (let read-expressions ((expressions '()))
		   (let ((next-expression (apply read args)))
		     (if (eof-object? next-expression) expressions
			 (read-expressions (cons next-expression expressions))))))))


    (define compile-number
      (lambda (num)
	(cond ((integer? num) (string-append "mk_integer(" (number->string num) ")"))
	      ((real? num) (string-append "mk_real(" (number->string num) ")"))
	      (else (error "compile error - unknown number type: " num)))))


    (define *foreign-intializations* '()) 
    (define *foreign-finalizations* '()) 
    (define *foreign-definitions* '()) 
    (define *foreign-declartions* '()) 



    (define compile-remainder
      (lambda (remainder source quote-level)
	(cond ((pair? remainder) (string-append "cons(" (compile-expression (car remainder) source quote-level) "," (compile-remainder (cdr remainder) source quote-level) ")"))
	      (else (compile-expression remainder source quote-level))
	      )))

    (define compile-expression
      (lambda (expression source quote-level)

	(letrec ((compile-time-macros `((include-string . ,(lambda (form)

							     (let* ((included-source (path-make-absolute (cadr form) (path-directory source)))
								    (included-string (with-input-from-file included-source read-string))
								    )

							       included-string
							       )))

					(foreign-declare . ,(lambda (form)

							      (let* ((included-string (apply string-append (map expand-compile-time-macro (cdr form))))
								     )

								(set! *foreign-declartions* (cons included-string *foreign-declartions*))
								;; #<unspecified>
								#t
								)))

					(foreign-define . ,(lambda (form)
							     (let* ((included-string (apply string-append (map expand-compile-time-macro (cdr form))))
								    )
							       (set! *foreign-definitions* (cons included-string *foreign-definitions*))
							       ;; #<unspecified> 
							       #t
							       )))

					(foreign-initialize . ,(lambda (form)
								 (let* ((included-string (apply string-append (map expand-compile-time-macro (cdr form))))
									)
								   (set! *foreign-intializations* (cons included-string *foreign-intializations*))
								   ;; #<unspecified> 
								   #t
								   )))

					(foreign-finalize . ,(lambda (form)
							       (let* ((included-string (apply string-append (map expand-compile-time-macro (cdr form))))
								      )
								 (set! *foreign-finalizations* (cons included-string *foreign-finalizations*))
								 ;; #<unspecified> 
								 #t
								 )))
					)
				      )
		 
		 (compile-time-macro? (lambda (form)
					(and (pair? form) (symbol? (car form)) (assoc (car form) compile-time-macros))))

		 (expand-compile-time-macro (lambda (form)

					      (if (compile-time-macro? form)
						  ((cdr (assoc (car form) compile-time-macros)) form)
						  form)))
		 
		 )


	  (cond ((and (pair? expression) (equal? (car expression) 'quote) (zero? quote-level))
		 (string-append "cons(mk_symbol(\"quote\")," (compile-remainder (cdr expression) source -1) ")"))

		((and (pair? expression) (equal? (car expression) 'quasiquote) (not (negative? quote-level)))
		 (string-append "cons(mk_symbol(\"quasiquote\")," (compile-remainder (cdr expression) source (+ quote-level 1)) ")"))

		((and (pair? expression) (equal? (car expression) 'unquote) (positive? quote-level))
		 (string-append "cons(mk_symbol(\"unquote\")," (compile-remainder (cdr expression) source (- quote-level 1)) ")"))

		((and (pair? expression) (equal? (car expression) 'unquote-splicing) (positive? quote-level))
		 (string-append "cons(mk_symbol(\"unquote-splicing\")," (compile-remainder (cdr expression) source (- quote-level 1)) ")"))



		((and (pair? expression) (zero? quote-level) (equal? (car expression) 'foreign-function))
		 (string-append "mk_function(" (object->string (cadr expression)) ",&NIL)" ))

		((and (pair? expression) (zero? quote-level) (equal? (car expression) 'foreign-operation))
		 (string-append "mk_operation(" (object->string (cadr expression)) ",&NIL)" ))

		((and (pair? expression) (zero? quote-level) (equal? (car expression) 'foreign-syntax))
		 (string-append "mk_syntax(" (object->string (cadr expression)) "," (object->string (caddr expression))  ")" ))

		((and (pair? expression) (zero? quote-level) (equal? (car expression) 'foreign-pointer))
		 (object->string (cadr expression)))

		((and (pair? expression) (zero? quote-level) (equal? (car expression) 'foreign-string))
		 (string-append "mk_string(" (cadr expression) ")" ))




		((and (pair? expression) (zero? quote-level) (equal? (car expression) 'include))
		 (let* ((included-source (path-make-absolute (cadr expression) (path-directory source)))
			(included-expressions (with-input-from-file included-source read-list))
			)
		   (compile-expression (cons '(foreign-syntax LOC_BEGIN "begin") included-expressions) included-source quote-level)))


		((and (zero? quote-level) (compile-time-macro? expression))
		 (let* ((expanded-expression (expand-compile-time-macro expression))
			(_quote-level (if (equal? expanded-expression expression) -1 quote-level)))
		   (compile-expression expanded-expression source _quote-level)
		   ))

		((pair? expression) (string-append "cons(" (compile-expression (car expression) source quote-level) "," (compile-remainder (cdr expression) source quote-level) ")"))
		((null? expression) (string-append "NIL"))
		((boolean? expression) (if expression (string-append "T") (string-append "F")))
		((char? expression) (string-append "mk_character(" (number->string (char->integer expression)) ")"))
		((symbol? expression) (string-append "mk_symbol(\"" (symbol->string expression) "\")"))
		((string? expression) (string-append "mk_string(" (object->string expression) ")" ))

		((number? expression) (compile-number expression))

		;; ((equal? expression #<undefined>) (string-append "UNDEF"))
		;; ((equal? expression #<unspecified>) (string-append "T"))


		(else (error "compile error - unknown expression type: " expression))
		))))


    (define module-name->c_name
      (lambda (module)
	(let ((valid-chars (string->list (string-append "abcdefghijklmnopqrstuvwxyz"
							"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
							"0123456789"
							"_"))))
	  (list->string (map (lambda (c)
			       (if (member c valid-chars) c
				   #\_))
			     (string->list module))))))

    

    (define module-loader-name
      (lambda (module-name)
	(string-append "LOAD_MODULE__" (module-name->c_name module-name))))
    
    (define module-loader-declaration
      (lambda (module-name)
	;; (string-append "pointer " (module-loader-name module-name) "(void)")
	(string-append "pointer " (module-loader-name module-name) "(pointer environment)")
	))

    (define module-loader-prototype
      (lambda (module-name)
	;; (string-append "pointer " (module-loader-name module-name) "()")
	(string-append "pointer " (module-loader-name module-name) "(pointer environment)")
	))
    

    (define compile-module-loader
      (lambda (name sources)

    	(let* ((statements "")
	       )
	  (for-each (lambda (source)
		      
		      (with-input-from-file 
			  source
			(lambda ()
			  (let process-expression ((expression (read)))
			    (if (not (eof-object? expression))
				(let ((next-expression (read)))
				   
				  (set! statements (string-append statements 
								  (if (eof-object? next-expression) "return_value = " "")
								  "autoscheme_eval(" (compile-expression expression source 0) ", environment);\n"))
				   (process-expression next-expression)))))

			))
		    sources)

    	  (string-append (module-loader-prototype name) "\n"
    			 "{\n"
			 "pointer return_value = T;\n"
			 
    			 (apply string-append *foreign-intializations*)

    			 statements

    			 (apply string-append *foreign-finalizations*)

    			 "return return_value;\n"
    			 "}\n"
			 
    			 ))))


    (define compile-module 
      (lambda (source-files module-name output-file)

	(set! *foreign-intializations* '()) 
	(set! *foreign-finalizations* '()) 
	(set! *foreign-definitions* '()) 
	(set! *foreign-declartions* '()) 

	(let* ((module-name (or module-name "module"))
	       (output-file (or output-file "module.c"))

	       (includes-template (string-append 
				   "#include \"autoscheme.h\"\n"
				   ))
	       (declarations-template (string-append
				       (module-loader-declaration module-name) ";\n"
				       ))
	       
	       (module-loader (compile-module-loader module-name source-files))

	       (output-port (open-output-file output-file))
	       )

	  (display includes-template output-port)
	  (display declarations-template output-port)

	  (display (apply string-append *foreign-declartions*) output-port)
	  (display (apply string-append *foreign-definitions*) output-port)

	  (display module-loader output-port)

	  (close-output-port output-port)
	  )))

    (define compile-program 
      (lambda (source-files module-list output-file)

	(set! *foreign-intializations* '()) 
	(set! *foreign-definitions* '()) 
	(set! *foreign-declartions* '()) 

	(let* ((includes-template (string-append 
				   "#include \"autoscheme.h\"\n"
				   ))
	       (declarations-template (string-append

				       (apply string-append (map (lambda (name)
								   (string-append (module-loader-declaration name) ";\n"))
								 module-list))

				       (module-loader-declaration "program") ";\n"
				       "\n"
				       ))

	       (functions-template (string-append

				    "int main( int argc, char **argv )\n"
				    "{\n"
				    "    scheme_init( argc, argv );\n"

				    (apply string-append (map (lambda (name)
								(string-append "    LOAD_MODULE__" name "(global_env);\n"))
							      module-list))

				    "    LOAD_MODULE__program(global_env);\n"
				    "    scheme_deinit();\n"
				    "    return 0;\n"
				    "}\n"
				    ))

	       (module-loader (compile-module-loader "program" source-files))
	       
	       (output-file (if output-file output-file "program.c"))
	       (output-port (open-output-file output-file))
	       )

	  (display includes-template output-port)
	  (display declarations-template output-port)
	  (display functions-template output-port)

	  (display (apply string-append *foreign-declartions*) output-port)
	  (display (apply string-append *foreign-definitions*) output-port)

	  (display module-loader output-port)

	  (close-output-port output-port)

	  )

	))


    ))