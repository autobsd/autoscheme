;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(define-library (auto scheme compile)
  (import (only (s7) object->string))

  (export compile-module
	  compile-program)

  (import (auto scheme path)
	  (auto scheme base)
 	  ;; (auto scheme process context)
	  )

  (begin

    (define compile-number
      (lambda (sc num)
	(cond ((integer? num) (string-append "mk_integer(" (number->string num) ")"))
	      ((real? num) (string-append "mk_real(" (number->string num) ")"))
	      (else (error "compile error - unknown number type: " num)))))


    (define *foreign-intializations* '()) 
    (define *foreign-finalizations* '()) 
    (define *foreign-definitions* '()) 
    (define *foreign-declartions* '()) 




    (define compile-expression
      (lambda (sc expression source quote-level)

	(letrec ((compile-time-macros `(;; (include . ,(lambda (form)
					;; 	      (if (null? (cdr form)) form
					;; 		  (let* ((included-source (path-make-absolute (cadr form) (path-directory source)))
					;; 			 (included-expressions (begin  (display "inside compile-time 'include' macro...")(newline)
					;; 						       (display "included-source: ")(write included-source)(newline)
					;; 						       (with-input-from-file included-source read-list)))
					;; 			 )

					;; 		    (cons 'begin included-expressions)
					;; 		    ))))

					(include-string . ,(lambda (form)
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
					))
		 (compile-time-macro? (lambda (form)
					(and (pair? form) (symbol? (car form)) (assoc (car form) compile-time-macros))))

		 (expand-compile-time-macro (lambda (form)
					      (if (compile-time-macro? form)
						  ((cdr (assoc (car form) compile-time-macros)) form)
						  form)))
		 
		 )


	  (cond ((and (pair? expression) (member (car expression) '(quote _quote)) (zero? quote-level))
		 (string-append "cons( mk_symbol(\"quote\" )," (compile-expression sc (cdr expression) source -1) ")"))

		((and (pair? expression) (member (car expression) '(quasiquote _quasiquote)) (not (negative? quote-level)))
		 (string-append "cons( mk_symbol(\"quasiquote\" )," (compile-expression sc (cdr expression) source (+ quote-level 1)) ")"))

		((and (pair? expression) (member (car expression) '(unquote _unquote)) (positive? quote-level))
		 (string-append "cons( mk_symbol(\"unquote\" )," (compile-expression sc (cdr expression) source (+ quote-level 1)) ")"))

		((and (pair? expression) (member (car expression) '(unquote-splicing _unquote-splicing)) (positive? quote-level))
		 (string-append "cons( mk_symbol(\"unquote-splicing\" )," (compile-expression sc (cdr expression) source (+ quote-level 1)) ")"))


		((and (pair? expression) (zero? quote-level) (equal? (car expression) 'include))
		 (let* ((included-source (path-make-absolute (cadr expression) (path-directory source)))
			(included-expressions (with-input-from-file included-source read-list))
			)
		   (compile-expression sc (cons 'begin included-expressions) included-source quote-level)))
		


		((and (zero? quote-level) (compile-time-macro? expression))
		 (let* ((expanded-expression (expand-compile-time-macro expression))
			(_quote-level (if (equal? expanded-expression expression) -1 quote-level)))
		   (compile-expression sc expanded-expression source _quote-level)
		   ))

		((pair? expression) (string-append "cons(" (compile-expression sc (car expression) source quote-level) "," (compile-expression sc (cdr expression) source quote-level) ")"))
		((null? expression) (string-append "NIL"))
		((boolean? expression) (if expression (string-append "T") (string-append "F")))
		((char? expression) (string-append "mk_character(" (number->string (char->integer expression)) ")"))

		;; ((equal? expression '_quote) (string-append "mk_symbol(\"quote\" )"))
		;; ((equal? expression '_quasiquote) (string-append "mk_symbol(\"quasiquote\" )"))
		;; ((equal? expression '_unquote) (string-append "mk_symbol(\"unquote\" )"))
		;; ((equal? expression '_unquote-splicing) (string-append "mk_symbol(\"unquote-splicing\" )"))

		((symbol? expression) (string-append "mk_symbol(\"" (symbol->string expression) "\")"))
		((string? expression) (string-append "mk_string(" (object->string expression) ")" ))

		((number? expression) (compile-number sc expression))

		((equal? expression #<undefined>) (string-append "UNDEF"))
		((equal? expression #<unspecified>) (string-append "T"))


		(else (error "compile error - unknown expression type: " expression))
		))))


    (define module-prototype
      (lambda (name)
	;; (string-append "int " "autoscheme_module__" name "( s7_scheme *s7, s7_pointer env )")))
	(string-append "int " "LOAD_MODULE__" name "()")))



    (define compile-module-function
      (lambda (name sources)
	(let* ((sc "s7")
	       (evaluated-expressions (string-append  "scheme_eval(cons(mk_symbol(\"begin\")," 
						      (let get-expressions ((remainder sources)
									    )
							(cond ((null? remainder) (string-append "NIL" ))

							      (else (string-append "cons(" (compile-expression sc (cons 'begin (with-input-from-file (car remainder) read-list)) (path-make-absolute (car remainder)) 0) ","
										   (get-expressions (cdr remainder))
										   ")"
										   ))
							      )
							)

						      "));\n"))
	       

	       )

	  (string-append (module-prototype name) "\n"
			 "{\n"


			 (apply string-append *foreign-intializations*)
			 

			 evaluated-expressions

			 (apply string-append *foreign-finalizations*)

			 "return 0;\n"
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
				   "#define _Bool int\n"
				   "#include \"autoscheme.h\"\n"
				   ))
	       (declarations-template (string-append
				       (module-prototype module-name) ";\n"
				       ))
	       
	       (module-function (compile-module-function module-name source-files))

	       (output-port (open-output-file output-file))
	       )

	  (display includes-template output-port)
	  (display declarations-template output-port)

	  (display (apply string-append *foreign-declartions*) output-port)
	  (display (apply string-append *foreign-definitions*) output-port)

	  (display module-function output-port)

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
								   (string-append (module-prototype name) ";\n"))
								 module-list))

				       (module-prototype "program") ";\n"

				       "int auto_argc;\n"
				       "char **auto_argv;\n"

				       ))

	       (functions-template (string-append

				    ;; "s7_scheme *auto_init()\n"
				    ;; "{\n"
				    ;; "    s7_scheme *s7 = s7_init();\n"

				    ;; "mod_env = s7_inlet( s7, s7_f( s7 ));\n"
				    ;; "mod_env_loc = s7_gc_protect( s7, mod_env );\n"

				    ;; "    return s7;\n"
				    ;; "}\n"

				    "int main( int argc, char **argv )\n"
				    "{\n"

				    "scheme_init();\n"

				    ;; "    s7_scheme *s7 = auto_init();\n"

				    "    auto_argc = argc;\n"
				    "    auto_argv = argv;\n"

				    (apply string-append (map (lambda (name)
								;; (string-append " autoscheme_module__" name "( s7, mod_env );\n"))
								(string-append " LOAD_MODULE__" name "();\n"))
							      module-list))

				    ;; "    autoscheme_module__program( s7, mod_env );\n"
				    "    LOAD_MODULE__program();\n"

				    ;; "s7_gc_unprotect_at( s7, mod_env_loc );\n"

				    ;; "s7_free( s7 );\n"

				    "scheme_deinit();\n"
				    "    return 0;\n"
				    "}\n"
				    ))

	       (module-function (compile-module-function "program" source-files))
	       
	       (output-file (if output-file output-file "program.c"))
	       (output-port (open-output-file output-file))
	       )

	  (display includes-template output-port)
	  (display declarations-template output-port)
	  (display functions-template output-port)

	  (display (apply string-append *foreign-declartions*) output-port)
	  (display (apply string-append *foreign-definitions*) output-port)

	  (display module-function output-port)

	  (close-output-port output-port)

	  )

	))


    ))