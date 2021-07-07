(display "compiling file...")(newline)



(define scheme-program->c-function 
  (lambda (program)
    (let* ((program-string (with-output-to-string (lambda () (write program))))
	   (quoted-program-string (with-output-to-string (lambda () (write program-string))))
	   (c-function (string-append

			"int program( s7_scheme *s7 )\n"
			"{\n"
			"char *program_str = " quoted-program-string  ";\n"
			"s7_eval_c_string( s7, program_str );\n"

			"return 0;\n"
			"}\n"

			)))
      c-function
      )))



(let* ((program-includes (string-append 
			  "#define _Bool int\n"
			  "#include \"s7/s7.h\"\n"
			  "int auto_argc; char **auto_argv;\n"
			  "int program( s7_scheme *s7 );\n"
			  ))
       (main-function (string-append "int main( int argc, char **argv )\n"
				     "{\n"
				     "s7_scheme *s7 = s7_init();\n"

				     "auto_argc = argc;\n"
				     "auto_argv = argv;\n"

				     "return program( s7 );\n"
				     "}\n"
				     ))

       (input-file (cadr (command-line)))
       (input-port (open-input-file input-file))

       (program (cons 'begin (reverse (let read-program ((expressions '()))
					(let ((next-expression (read input-port)))
					  (if (eof-object? next-expression) expressions
					      (read-program (cons next-expression expressions))))))))

       (c-function (scheme-program->c-function program))

       (output-file (string-append input-file ".c"))
       (output-port (open-output-file output-file))
       )


  (close-input-port input-port)

  (display program-includes output-port)
  (display c-function output-port)
  (display main-function output-port)

  (close-output-port output-port)
  )