(display "compiling file...")(newline)
(set! (*s7* 'print-length) 1024)


(define expand
  (lambda (expression env)
    (cond ((not (pair? expression)) expression)
	  (else (varlet env 'expression expression)
		(with-let env (let* ((first (car expression))
				     (expanded-first (cond ((not (pair? first)) first)
							   ((and (equal? (car first) 'define-macro)) (eval first) #<unspecified>)
							   ((and (symbol? (car first))(macro? (symbol->value (car first)))) (expand (apply macroexpand (list first)) (sublet (curlet))))
							   (else (cons (expand (car first) (sublet (curlet)))
								       (expand (cdr first) (sublet (curlet)))))))
				     )
				(cons expanded-first
				      (expand (cdr expression) (sublet (curlet)))
				      ))))))
  )


(define compile-number
  (lambda (sc num)
    (cond ((integer? num) (string-append "s7_make_integer(" sc "," (number->string num) ")"))
	  (else (error "compile error - unknown number type: " num)))))

(define compile
  (lambda (sc expression)
    (cond ((pair? expression) (string-append "s7_cons(" sc "," (compile sc (car expression)) "," (compile sc (cdr expression)) ")"))
	  ((null? expression) (string-append "s7_nil(" sc ")"))

	  ((symbol? expression) (string-append "s7_make_symbol(" sc ",\"" (symbol->string expression) "\")"))
	  ((string? expression) (string-append "s7_make_string(" sc ",\"" expression "\")" ))
	  ((number? expression) (compile-number sc expression))
	  (else (error "compile error - unknown type: " expression))
	  )))


(define module-prototype
  (lambda (name)
    (string-append "s7_pointer " name "_module( s7_scheme *s7 )")))


(define display-module
  (lambda (name expressions port)
    (let* ((expression (cons 'begin expressions))
	   (compiled-expression (compile "s7" expression))
	   )
      (display (string-append (module-prototype name) "\n"
			      "{\n"
			      "return("
			      compiled-expression
			      ");\n"
			      "}\n"
			      ) port)
      )))



(let* ((includes-template (string-append 
			   "#define _Bool int\n"
			   "#include \"s7/s7.h\"\n"
			   ))
       (declarations-template (string-append
			       "int auto_argc; char **auto_argv;\n"
			       "s7_scheme *auto_init( void );\n"
			       (module-prototype "scheme") ";\n"
			       (module-prototype "program") ";\n"
			       ))

       (functions-template (string-append
			    "static s7_pointer command_line( s7_scheme *sc, s7_pointer args )\n"
			    "{\n"
			    "    if( !s7_is_null( sc, args ))\n"
			    "	return s7_wrong_type_arg_error( sc, \"command-line\", 0, args, \"null\" );\n"
			    "    else\n"
			    "    {\n"
			    "	s7_pointer arguments = s7_nil( sc );\n"
			    "	int i;\n"
			    "	for( i = auto_argc - 1; i >= 0; i-- )\n"
			    "	{\n"
			    "	    arguments = s7_cons( sc, s7_make_string( sc, auto_argv[i] ), arguments );\n"
			    "	}\n"
			    "	return arguments;\n"
			    "    }\n"
			    "}\n"
			    "s7_scheme *auto_init()\n"
			    "{\n"
			    "    s7_scheme *s7 = s7_init();\n"
			    "    s7_define_function( s7, \"command-line\", command_line, 0, 0, false, \"(command-line) returns a list of command-line arguments\" );\n"
			    "    return s7;\n"
			    "}\n"

			    "int main( int argc, char **argv )\n"
			    "{\n"
			    "    s7_scheme *s7 = auto_init();\n"

			    "    auto_argc = argc;\n"
			    "    auto_argv = argv;\n"

			    "    s7_eval( s7, scheme_module( s7 ), s7_f( s7 ));\n"
			    "    s7_eval( s7, program_module( s7 ), s7_f( s7 ));\n"

			    "    return 0;\n"
			    "}\n"
			    ))

       (scheme-expressions (load "src/scheme.scm"))

       (input-file (cadr (command-line)))
       (input-port (open-input-file input-file))


       ;; (program (expand (append scheme (reverse (let read-program ((expressions '()))
       ;; 						  (let ((next-expression (read input-port)))
       ;; 						    (if (eof-object? next-expression) expressions
       ;; 							(read-program (cons next-expression expressions)))))))
       ;; 			(sublet (curlet)))
       ;; 		)

       (program-expressions (reverse (let read-program ((expressions '()))
				       (let ((next-expression (read input-port)))
					 (if (eof-object? next-expression) expressions
					     (read-program (cons next-expression expressions)))))))


       (output-file (string-append input-file ".c"))
       (output-port (open-output-file output-file))

       )


  (close-input-port input-port)

  (display includes-template output-port)
  (display declarations-template output-port)
  (display functions-template output-port)

  (display-module "scheme" scheme-expressions output-port)
  (display-module "program" program-expressions output-port)
  
  (close-output-port output-port)

  )

