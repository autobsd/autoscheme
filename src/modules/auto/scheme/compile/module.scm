(define-library (auto scheme compile)
  (export compile-module
	  compile-program)

  (import (auto scheme path))

  (begin

    (define compile-number
      (lambda (sc num)
	(cond ((integer? num) (string-append "s7_make_integer(" sc "," (number->string num) ")"))
	      (else (error "compile error - unknown number type: " num)))))


    (define compile-expression
      (lambda (sc expression source quote-level)
	(cond ((and (pair? expression) (equal? (car expression) 'quote) (zero? quote-level))
	       (string-append "s7_cons(" sc ",s7_make_symbol(" sc ",\"quote\")," (compile-expression sc (cdr expression) source -1) ")"))

	      ((and (pair? expression) (equal? (car expression) 'quasiquote) (not (negative? quote-level)))
	       (string-append "s7_cons(" sc ",s7_make_symbol(" sc ",\"quasiquote\")," (compile-expression sc (cdr expression) source (+ quote-level 1)) ")"))

	      ((and (pair? expression) (equal? (car expression) 'unquote) (positive? quote-level))
	       (string-append "s7_cons(" sc ",s7_make_symbol(" sc ",\"unquote\")," (compile-expression sc (cdr expression) source (1 quote-level 1)) ")"))


	      ((and (pair? expression) (equal? (car expression) 'include) (zero? quote-level)) 
	       (let* ((included-source (path-make-absolute (cadr expression) (path-directory source)))
		      (included-expressions (get-expressions-from-file included-source))
		      )
		 (compile-expression sc (cons 'begin included-expressions) included-source quote-level)
		 ))

	      ((and (pair? expression) (equal? (car expression) 'include-string) (zero? quote-level)) 
	       (let* ((included-source (path-make-absolute (cadr expression) (path-directory source)))
		      (included-string (with-input-from-file included-source 
					 (lambda () 
					   (let get-string((s (read-string 5)))
					     (if (eof-object? s) 
						 ""
						 (string-append s (get-string (read-string 5)))))
					   )))
		      )
		 (compile-expression sc included-string source quote-level)
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
	      )))


    (define module-prototype
      (lambda (name)
	(string-append "int " "autoscheme_module__" name "( s7_scheme *s7, s7_pointer env )")))


    (define get-expressions-from-file
      (lambda (input-file)
	(let* ((input-port (open-input-file input-file))
	       (expressions (reverse (let read-expressions ((expressions '()))
				       (let ((next-expression (read input-port)))
					 (if (eof-object? next-expression) expressions
					     (read-expressions (cons next-expression expressions)))))))
	       )
	  (close-input-port input-port)
	  expressions)))




    (define compile-module-function
      (lambda (name sources)
	(let ((sc "s7")
	      )
	  (string-append (module-prototype name) "\n"
			 "{\n"
			 "s7_eval(" sc ",s7_cons(" sc ",s7_make_symbol(" sc ",\"begin\")," 


	  (let get-expressions ((remainder sources)
				    )
	    (cond ((null? remainder) (string-append "s7_nil(" sc ")" ))

		  (else (string-append "s7_cons(" sc "," (compile-expression sc (cons 'begin (get-expressions-from-file (car remainder))) (path-make-absolute (car remainder)) 0) ","
				       (get-expressions (cdr remainder))
				       ")"
			))
		  )
	    )

	  "),env);\n"
	  "return 0;" 
	  "}\n"
	
	  ))))





    (define compile-module 
      (lambda (source-files module-name output-file)
	(display "compiling module...")(newline)
	(display "module-name:")(write module-name)(newline)
	(display "output-file:")(write output-file)(newline)
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
	  (display module-function output-port)

	  (close-output-port output-port)
	  )))

    (define compile-program 
      (lambda (source-files module-list output-file)

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
				       ;; (module-prototype "scheme") ";\n"
				       
				       (apply string-append (map (lambda (name)
								   (string-append (module-prototype name) ";\n"))
								 module-list))

				       (module-prototype "program") ";\n"
				       ))

	       (functions-template (string-append

				    (include-string "command-line.c")
				    (include-string "current-directory.c")

				    "s7_scheme *auto_init()\n"
				    "{\n"
				    "    s7_scheme *s7 = s7_init();\n"

				    "mod_env = s7_inlet( s7, s7_f( s7 ));\n"
				    "mod_env_loc = s7_gc_protect( s7, mod_env );\n"

				    "    s7_define_function( s7, \"command-line\", command_line, 0, 0, false, \"(command-line) returns a list of command-line arguments\" );\n"
				    "    s7_define_function( s7, \"current-directory\", current_directory, 0, 0, false, \"(current-directory) returns the current working directory\" );\n"
				    "    return s7;\n"
				    "}\n"

				    "int main( int argc, char **argv )\n"
				    "{\n"
				    "    s7_scheme *s7 = auto_init();\n"

				    "    auto_argc = argc;\n"
				    "    auto_argv = argv;\n"


				    (apply string-append (map (lambda (name)
								;; (string-append " s7_eval( s7, autoscheme_module__" name "( s7 ), mod_env );\n"))
								(string-append " autoscheme_module__" name "( s7, mod_env );\n"))
							      module-list))



				    ;; "    s7_eval( s7, scheme_module( s7 ), s7_f( s7 ));\n"
				    ;; "    s7_eval( s7, autoscheme_module__program( s7 ), mod_env );\n"
				    "    autoscheme_module__program( s7, mod_env );\n"


				    "s7_gc_unprotect_at( s7, mod_env_loc );\n"

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
	  (display module-function output-port)

	  (close-output-port output-port)

	  )

	))


))