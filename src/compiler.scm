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



(define display-program-function 
  (lambda (program port)
    (let* ((program-string (object->string program))
	   )


      (display (string-append "int program( s7_scheme *s7 )\n"
			      "{\n"
			      "const char program_str[] = { " ) port)

      (with-input-from-string program-string (lambda ()
					       (let read-chars ()
						 (let ((c (read-char)))
						   (cond ((eof-object? c) (display "'\\0'" port))
							 ((equal? c #\') (display "'\\'', " port ) (read-chars))
							 (else (display "'" port)(display c port)(display "', " port)(read-chars)))))))


      (display (string-append " };\n"
			      "s7_eval_c_string( s7, program_str );\n"

			      "return 0;\n"
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
			       "int program( s7_scheme *s7 );\n"
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

			    "    return program( s7 );\n"
			    "}\n"
			    ))

       (scheme (load "src/scheme.scm"))

       (input-file (cadr (command-line)))
       (input-port (open-input-file input-file))


       (program (expand (append scheme (reverse (let read-program ((expressions '()))
						  (let ((next-expression (read input-port)))
						    (if (eof-object? next-expression) expressions
							(read-program (cons next-expression expressions)))))))
			(sublet (curlet)))
		)

       (output-file (string-append input-file ".c"))
       (output-port (open-output-file output-file))

       )


  (close-input-port input-port)

  (display includes-template output-port)
  (display declarations-template output-port)
  (display functions-template output-port)

  (display-program-function program output-port)
  
  (close-output-port output-port)

  )

