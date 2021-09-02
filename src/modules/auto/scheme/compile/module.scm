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
	(cond ((integer? num) (string-append "s7_make_integer(" sc "," (number->string num) ")"))
	      ((real? num) (string-append "s7_make_real(" sc "," (number->string num) ")"))
	      (else (error "compile error - unknown number type: " num)))))


    (define *foreign-intializations* #<unspecified>) 
    (define *foreign-definitions* #<unspecified>) 
    (define *foreign-declartions* #<unspecified>) 




    (define compile-expression
      (lambda (sc expression source quote-level)


	(letrec ((compile-time-macros `((include . ,(lambda (form)
						      (if (null? (cdr form)) form
							  (let* ((included-source (path-make-absolute (cadr form) (path-directory source)))
								 (included-expressions (with-input-from-file included-source read-list))
								 )
							    (cons 'begin included-expressions)
							    ))))

					(include-string . ,(lambda (form)
							     (let* ((included-source (path-make-absolute (cadr form) (path-directory source)))
								    (included-string (with-input-from-file included-source read-string))
								    )
							       included-string
							       )))

					(foreign-declaration . ,(lambda (form)
								  (let* ((included-string (apply string-append (map expand-compile-time-macro (cdr form))))
									 )
								    (set! *foreign-declartions* (cons included-string *foreign-declartions*))
								    #<unspecified>
								    )))

					(foreign-definition . ,(lambda (form)
								 (let* ((included-string (apply string-append (map expand-compile-time-macro (cdr form))))
									)
								   (set! *foreign-definitions* (cons included-string *foreign-definitions*))
								   #<unspecified> 
								   )))

					(foreign-initialization . ,(lambda (form)
								     (let* ((included-string (apply string-append (map expand-compile-time-macro (cdr form))))
									    )
								       (set! *foreign-intializations* (cons included-string *foreign-intializations*))
								       #<unspecified> 
								       )))
					))
		 (compile-time-macro? (lambda (form)
					(and (pair? form) (symbol? (car form)) (assoc (car form) compile-time-macros))))

		 (expand-compile-time-macro (lambda (form)
					      (if (compile-time-macro? form)
						  ((cdr (assoc (car form) compile-time-macros)) form)
						  form)))
		 
		 )


	  (cond ((and (pair? expression) (equal? (car expression) 'quote) (not (negative? quote-level)))
		 (string-append "s7_cons(" sc ",s7_make_symbol(" sc ",\"quote\")," (compile-expression sc (cdr expression) source -1) ")"))

		((and (pair? expression) (equal? (car expression) 'quasiquote) (not (negative? quote-level)))
		 (string-append "s7_cons(" sc ",s7_make_symbol(" sc ",\"quasiquote\")," (compile-expression sc (cdr expression) source (+ quote-level 1)) ")"))

		((and (pair? expression) (equal? (car expression) 'unquote) (positive? quote-level))
		 (string-append "s7_cons(" sc ",s7_make_symbol(" sc ",\"unquote\")," (compile-expression sc (cdr expression) source (1 quote-level 1)) ")"))

		((and (zero? quote-level) (compile-time-macro? expression))
		 (let* ((expanded-expression (expand-compile-time-macro expression))
			(_quote-level (if (equal? expanded-expression expression) -1 quote-level)))
		   (compile-expression sc expanded-expression source _quote-level)
		 ))

		((pair? expression) (string-append "s7_cons(" sc "," (compile-expression sc (car expression) source quote-level) "," (compile-expression sc (cdr expression) source quote-level) ")"))
		((null? expression) (string-append "s7_nil(" sc ")"))
		((boolean? expression) (if expression (string-append "s7_t(" sc ")") (string-append "s7_f(" sc ")")))
		((char? expression) (string-append "s7_make_character(" sc "," (number->string (char->integer expression)) ")"))
		((symbol? expression) (string-append "s7_make_symbol(" sc ",\"" (symbol->string expression) "\")"))
		((string? expression) (string-append "s7_make_string(" sc "," (object->string expression) ")" ))

		((number? expression) (compile-number sc expression))

		((equal? expression #<undefined>) (string-append "s7_undefined(" sc ")"))
		((equal? expression #<unspecified>) (string-append "s7_unspecified(" sc ")"))


		(else (error "compile error - unknown expression type: " expression))
		))))


    (define module-prototype
      (lambda (name)
	(string-append "int " "autoscheme_module__" name "( s7_scheme *s7, s7_pointer env )")))



    (define compile-module-function
      (lambda (name sources)
	(let* ((sc "s7")
	       (evaluated-expressions (string-append  "s7_eval(" sc ",s7_cons(" sc ",s7_make_symbol(" sc ",\"begin\")," 
						      (let get-expressions ((remainder sources)
									    )
							(cond ((null? remainder) (string-append "s7_nil(" sc ")" ))

							      (else (string-append "s7_cons(" sc "," (compile-expression sc (cons 'begin (with-input-from-file (car remainder) read-list)) (path-make-absolute (car remainder)) 0) ","
										   (get-expressions (cdr remainder))
										   ")"
										   ))
							      )
							)

						      "),env);\n"))
	       

	       )

	  (string-append (module-prototype name) "\n"
			 "{\n"


			 (apply string-append *foreign-intializations*)
			 

			 evaluated-expressions

			 "return 0;" 
			 "}\n"
			 
			 ))))





    (define compile-module 
      (lambda (source-files module-name output-file)

	(set! *foreign-intializations* '()) 
	(set! *foreign-definitions* '()) 
	(set! *foreign-declartions* '()) 

	(let* ((module-name (or module-name "module"))
	       (output-file (or output-file "module.c"))

	       (includes-template (string-append 
				   "#define _Bool int\n"
				   "#include \"s7/s7.h\"\n"
				   ))
	       (declarations-template (string-append
				       (module-prototype module-name) ";\n"))
	       
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
				   "#define _Bool int\n"
				   "#include \"s7/s7.h\"\n"

				   "#include <stdio.h>\n"
				   "/*#define WINDOWS*/\n"
				   "#ifdef WINDOWS\n"
				   "#include <direct.h>\n"
				   "#define GetCurrentDir _getcwd\n"
				   "#else\n"
				   "#include <unistd.h>\n"
				   "#define GetCurrentDir getcwd\n"
				   "#endif\n"
				   ))
	       (declarations-template (string-append
				       "int auto_argc; char **auto_argv;\n"

				       "s7_int mod_env_loc;\n"
				       "s7_pointer mod_env;\n"

				       "s7_scheme *auto_init( void );\n"
				       
				       (apply string-append (map (lambda (name)
								   (string-append (module-prototype name) ";\n"))
								 module-list))

				       (module-prototype "program") ";\n"
				       ))

	       (functions-template (string-append

				    "s7_scheme *auto_init()\n"
				    "{\n"
				    "    s7_scheme *s7 = s7_init();\n"

				    "mod_env = s7_inlet( s7, s7_f( s7 ));\n"
				    "mod_env_loc = s7_gc_protect( s7, mod_env );\n"

				    "    return s7;\n"
				    "}\n"

				    "int main( int argc, char **argv )\n"
				    "{\n"
				    "    s7_scheme *s7 = auto_init();\n"

				    "    auto_argc = argc;\n"
				    "    auto_argv = argv;\n"

				    (apply string-append (map (lambda (name)
								(string-append " autoscheme_module__" name "( s7, mod_env );\n"))
							      module-list))

				    "    autoscheme_module__program( s7, mod_env );\n"

				    "s7_gc_unprotect_at( s7, mod_env_loc );\n"

				    "s7_free( s7 );\n"

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