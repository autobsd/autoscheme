(display "compiling file...")(newline)

(define display-program-function 
  (lambda (program port)
    (let* ((program-string (with-output-to-string (lambda () (write program))))
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

       (scheme (load "src/scheme.scm"))

       (input-file (cadr (command-line)))
       (input-port (open-input-file input-file))


       (program (append scheme (reverse (let read-program ((expressions '()))
       					  (let ((next-expression (read input-port)))
       					    (if (eof-object? next-expression) expressions
       						(read-program (cons next-expression expressions))))))))

       (output-file (string-append input-file ".c"))
       (output-port (open-output-file output-file))

       )

  (close-input-port input-port)

  (display program-includes output-port)
  (display-program-function program output-port)
  (display main-function output-port)
  
  (close-output-port output-port)

  )

