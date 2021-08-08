(define compile-number
  (lambda (sc num)
    (cond ((integer? num) (string-append "s7_make_integer(" sc "," (number->string num) ")"))
	  (else (error "compile error - unknown number type: " num)))))


(define compile
  (lambda (sc expression)
    (cond ((pair? expression) (string-append "s7_cons(" sc "," (compile sc (car expression)) "," (compile sc (cdr expression)) ")"))
	  ((null? expression) (string-append "s7_nil(" sc ")"))
	  ((boolean? expression) (if expression (string-append "s7_t(" sc ")") (string-append "s7_f(" sc ")")))
	  ((char? expression) (string-append "s7_make_character(" sc "," (number->string (char->integer expression)) ")"))
	  ((symbol? expression) (string-append "s7_make_symbol(" sc ",\"" (symbol->string expression) "\")"))
	  ((string? expression) (string-append "s7_make_string(" sc "," (object->string expression) ")" ))

	  ((number? expression) (compile-number sc expression))

	  ((equal? expression #<undefined>) (string-append "s7_undefined(" sc ")"))
	  ((equal? expression #<unspecified>) (string-append "s7_unspecified(" sc ")"))


	  (else (error "compile error - unknown type: " expression))
	  )))


(define module-prototype
  (lambda (name)
    (string-append "s7_pointer " "autoscheme_module__" name "( s7_scheme *s7 )")))


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


(define display-module
  (lambda (name sources port)
    (let* ((expression (cons 'begin (apply append (map get-expressions-from-file sources))))
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
	   (output-port (open-output-file output-file))
	   )
      (display includes-template output-port)

      (display declarations-template output-port)
      (display-module module-name source-files output-port)
      
      (close-output-port output-port)
      )))

(define compile-program 
  (lambda (source-files module-list output-file)

    

    (let* ((includes-template (string-append 
			       "#define _Bool int\n"
			       "#include \"s7/s7.h\"\n"
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

				"mod_env = s7_inlet( s7, s7_f( s7 ));\n"
				"mod_env_loc = s7_gc_protect( s7, mod_env );\n"

				"    s7_define_function( s7, \"command-line\", command_line, 0, 0, false, \"(command-line) returns a list of command-line arguments\" );\n"
				"    return s7;\n"
				"}\n"

				"int main( int argc, char **argv )\n"
				"{\n"
				"    s7_scheme *s7 = auto_init();\n"

				"    auto_argc = argc;\n"
				"    auto_argv = argv;\n"


				(apply string-append (map (lambda (name)
							    (string-append " s7_eval( s7, autoscheme_module__" name "( s7 ), mod_env );\n"))
							  module-list))



				;; "    s7_eval( s7, scheme_module( s7 ), s7_f( s7 ));\n"
				"    s7_eval( s7, autoscheme_module__program( s7 ), mod_env );\n"


				"s7_gc_unprotect_at( s7, mod_env_loc );\n"

				"    return 0;\n"
				"}\n"
				))

	   (output-file (if output-file output-file "program.c"))
	   (output-port (open-output-file output-file))

	   )

      (display includes-template output-port)
      (display declarations-template output-port)
      (display functions-template output-port)
      (display-module "program" source-files output-port)
      
      (close-output-port output-port)

      )

    ))



;; (varlet *source* 'directory
;; 	    (lambda ()

;; 	      (let ((pos -1)
;; 		    )
;; 		(do ((len (length (let-ref *source* 'path)))
;; 		     (i 0 (+ i 1)))
;; 		    ((= i len))
;; 		  (if (char=? ((let-ref *source* 'path) i) #\/)
;; 		      (set! pos i)))
;; 		(if (positive? pos)
;; 		    (let ((directory-name (substring (let-ref *source* 'path) 0 pos)))
;; 		      directory-name)))))

;; (varlet *source* 'absolute?
;; 	    (lambda (path)
;; 	      (char=? (path 0) #\/)))


;; (varlet *source* 'make-absolute
;; 	    (lambda (path)
;; 	      (if ((let-ref *source* 'absolute?) path) path
;; 		  (string-append ((let-ref *source* 'directory)) "/" path))))

;; (varlet *source* 'previous #<undefined> 'value #<undefined> )

;; (define-macro (include . filenames)
;;   (let ()
;; 	(cons 'begin (map (lambda (filename)
;; 			    `(begin (let-set! *source* 'previous (let-ref *source* 'path))
;; 				    (let-set! *source* 'path ,((let-ref *source* 'make-absolute) filename))
;; 				    (let-set! *source* 'value (load ,(string-append ((let-ref *source* 'directory)) "/" filename)))
;; 				    (let-set! *source* 'path (let-ref *source* 'pevious))
;; 				    (let-ref *source* 'value)))

;; 			  filenames)
;; 	      )))



;; (define expand
;;   (lambda (expression env)
;; 	(cond ((not (pair? expression)) expression)
;; 	      (else (varlet env 'expression expression)
;; 		    (with-let env (let* ((first (car expression))
;; 					 (expanded-first (cond ((not (pair? first)) first)
;; 							       ((and (equal? (car first) 'define-macro)) (eval first) #<unspecified>)
;; 							       ((and (symbol? (car first))(macro? (symbol->value (car first)))) (expand (apply macroexpand (list first)) (sublet (curlet))))
;; 							       (else (cons (expand (car first) (sublet (curlet)))
;; 									   (expand (cdr first) (sublet (curlet)))))))
;; 					 )
;; 				    (cons expanded-first
;; 					  (expand (cdr expression) (sublet (curlet)))
;; 					  ))))))
;;   )


















