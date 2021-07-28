(import (scheme write))
(import (scheme read))
(import (scheme base))
(import (scheme load))

(define string-join
  (lambda (string-list . rest)

    (let ((delimeter (if (pair? rest) (car rest) " "))
	  (grammar (if (and (pair? rest)(pair? (cdr rest))) (cadr rest) 'infix))
	  )

      (if (and (equal? grammar 'strict-infix) (null? string-list)) (error "cannot join with 'strict-infix" string-list))

      (let join-strings ((remainder string-list)
			 )
	(cond ((null? remainder) "")
	      ((equal? grammar 'suffix) (string-append (car remainder) delimeter (join-strings (cdr remainder))))
	      ((equal? grammar 'prefix) (string-append delimeter (car remainder) (join-strings (cdr remainder))))

 	      ((pair? (cdr remainder)) (string-append (car remainder) delimeter (join-strings (cdr remainder))))
	      (else (car remainder)))))))


;; (define fold-left 
;;   (lambda (f init seq) 
;;     (if (null? seq) 
;; 	init 
;; 	(fold-left f 
;; 		   (f init (car seq)) 
;; 		   (cdr seq)))))

;; (define fold-right 
;;   (lambda (f init seq) 
;;     (if (null? seq) 
;; 	init 
;; 	(f (car seq) 
;; 	   (fold-right f init (cdr seq))))))

;; (define fold fold-left)

(define option 
  (lambda (names required-arg? optional-arg? processor)
    (inlet 'names names
	   'required-arg? required-arg?
	   'optional-arg? optional-arg?
	   'processor processor)
    ))

(define option-names (lambda (option) (let-ref option 'names)))
(define option-required-arg? (lambda (option) (let-ref option 'required-arg?)))
(define option-optional-arg? (lambda (option) (let-ref option 'optional-arg?)))
(define option-processor (lambda (option) (let-ref option 'processor)))

(define args-fold-remainder
  (lambda (remainder options unrecognized-option-proc operand-proc end-of-options . seeds)

    (letrec* ((argument (if (pair? remainder) (car remainder) #f))
	      (argument-length (if argument (string-length argument)))

	      (find-option (lambda (name) (call/cc (lambda (return) (for-each (lambda (opt) (if (member name (option-names opt)) (return opt))) options)(return #f)))))

	      (process-option (lambda () (cond ((equal? (argument 1) #\-) (process-long-option)) (else (process-short-option)))))

	      (process-short-option (lambda ()
				      (let* ((opt-name (argument 1))
					     (recognized-option (find-option opt-name))
					     (opt-arg-allowed (or (not recognized-option) (option-required-arg? recognized-option) (option-optional-arg? recognized-option)))
					     (opt-arg-string (if (> argument-length 2) (substring argument 2 (string-length argument)) #f))

					     (updated-remainder (cdr remainder))

					     (opt-arg (cond ((not opt-arg-string)
							     (cond ((and opt-arg-allowed (pair? (cdr remainder)) (not (equal? ((cadr remainder) 0) #\-)))
								    (set! updated-remainder (cddr remainder)) (cadr remainder))
								   (else #f)))

							    ((or (not opt-arg-allowed) (find-option (opt-arg-string 0)))
							     (set! updated-remainder (cons (string-append "-" opt-arg-string) (cdr remainder))) #f)

							    (else opt-arg-string)
							    ))
					     
					     (result (list (if recognized-option 
							       ((option-processor recognized-option) recognized-option opt-name opt-arg (apply values seeds))
							       (unrecognized-option-proc recognized-option opt-name opt-arg (apply values seeds)))))
					     )

					(args-fold-remainder updated-remainder options unrecognized-option-proc operand-proc end-of-options (apply values result))
					)
				      
				      ))

	      (process-long-option (lambda ()
				     (let* ((name-end (let find-end ((pos 2))
							(cond ((equal? (string-length argument) pos) pos)
							      ((equal? (argument pos) #\=) pos)
							      (else (find-end (+ pos 1))))))
					    (opt-name (substring argument 2 name-end))
					    (recognized-option (find-option opt-name))
					    (opt-arg-string (if (< name-end (string-length argument))
								(substring argument (+ name-end 1) (string-length argument))
								#f))

					    (updated-remainder (cdr remainder))
					    (opt-arg opt-arg-string)
					    
					    (result (list (if recognized-option 
							      ((option-processor recognized-option) recognized-option opt-name opt-arg (apply values seeds))
							      (unrecognized-option-proc recognized-option opt-name opt-arg (apply values seeds)))))
					    )

				       (args-fold-remainder updated-remainder options unrecognized-option-proc operand-proc end-of-options (apply values result ))
				       )))
	      
	      (process-operand (lambda ()
				 (let ((result (list (operand-proc argument (apply values seeds)))))
				   (args-fold-remainder (cdr remainder) options unrecognized-option-proc operand-proc end-of-options (apply values result )))))
	      

	      )

	     (cond ((not argument) (apply values seeds))
		   (end-of-options (process-operand))
		   ((equal? argument "--") (args-fold-remainder (cdr remainder) options unrecognized-option-proc operand-proc #t (apply values seeds)))
		   ((<= (string-length argument) 1) (process-operand))
		   ((equal? (argument 0) #\-) (process-option))
		   (else (process-operand))
		   )
	     
	     )))


(define args-fold
  (lambda (args options unrecognized-option-proc operand-proc . seeds)
    (args-fold-remainder args options unrecognized-option-proc operand-proc #f (apply values seeds))))







(define display-usage
  (lambda (opt-tab)
    (display "Usage: autoscheme [options...] [sources...]")(newline)

    (let* ((max-short 0)
	   (max-long 0)
	   ;; (max-description 0)

	   (opt-strings (map (lambda (row)
			       (let* ((opt (car row))
				      (names (option-names opt))
				      
				      (short-names (apply append (map (lambda (name) (if (char? name) (list name) '())) names)))
				      (long-names (apply append (map (lambda (name) (if (string? name) (list name) '())) names)))

				      (name-delimiter (if (or (null? short-names)(null? long-names)) "  " ", "))

				      (short-string (string-join (map (lambda (short-name) (string-append "-" (string short-name))) short-names) ", " ))
				      (long-string (string-join (map (lambda (long-name) (string-append "--" long-name)) long-names) ", " ))
				      (description (cadr row))

				      (short-length (string-length short-string))
				      (long-length (string-length long-string))
				      ;; (description-length (string-length description))
				      )
				 (cond ((> short-length max-short) (set! max-short short-length))
				       ((> long-length max-long) (set! max-long long-length))
				       ;; ((> description-length max-description) (set! max-description description-length))
				       )

				 (list short-string name-delimiter long-string description)

				 ))
			     opt-tab)
			)
	   )
      (map (lambda (row)
	     (let* ((short-string (car row))
		    (delimiter (cadr row))
		    (long-string (caddr row))
		    (description (cadddr row))
		    (padding-short (make-string (- max-short (string-length short-string)) #\space))
		    (padding-long (make-string (- max-long (string-length long-string)) #\space))
		    
		    )
	       (display (string-append " " padding-short short-string delimiter long-string padding-long "  " description))(newline)

	       ))
	   opt-strings)

      )
    ))

(define display-version
  (lambda ()
    (display (string-append "AutoScheme version " "0.31.0 (rev 1627370131)"))(newline)
    ))

(define help-processor
  (lambda args
    (display-version)
    (display-usage option-table)
    (exit) ))

(define version-processor
  (lambda args
    (display-version)
    (exit) ))

(define recognized-processor 
  (lambda (option name arg . seeds)
    (let ((options (car seeds))
	  (source-files (cadr seeds)))

      (values (cons (list option name arg) options) source-files)

      )))




(define unrecognized-processor 
  (lambda (option name arg . seeds)
    (let ((name-string (if (char? name) 
			   (string-append "-" (string name))
			   (string-append "--" name)))
	  )

      (display (string-append "autoscheme: unrecognized option " name-string))(newline)
      (display-usage option-table)
      (exit 1) )))


(define operand-processor 
  (lambda (operand . seeds)
    (let ((options (car seeds))
	  (source-files (cadr seeds)))

      (values options (cons operand source-files)))))


(define option-table `((,(option '(#\i "interpret") #f #f recognized-processor) "Interpret sources with linked modules")
		       (,(option '(#\c "compile") #f #f recognized-processor) "Compile sources with linked modules")
		       (,(option '(#\l "link-modules") #f #f recognized-processor) "Link modules")
		       (,(option '(#\m "compile-module") #f #f recognized-processor) "Compile module")
		       (,(option '(#\n "module-name") #f #f recognized-processor) "Specify compiled module name")
		       (,(option '(#\r "repl") #f #f version-processor) "Enter interactive REPL")
		       (,(option '(#\s "shell") #f #f version-processor) "Enter command line shell")
		       (,(option '(#\V "version") #f #f version-processor) "Display version information")
		       (,(option '(#\h "help") #f #f help-processor) "Show this message")
		       ))



(define compile-program 
  (lambda (source-files)
    (display "compiling program with files: ")(write source-files)(newline)
    (define s7_object->string object->string)
    (define object->string
      (lambda (obj)
	(let ((str (s7_object->string obj)))
	  (if (string? obj) 
	      (with-output-to-string (lambda ()
				       (do ((i 0 (+ i 1)))
					   ((= i (string-length str)) )      
					 (cond ((member (str i) `(,("\n" 0) #\newline))(display "\\n"))
					       (else (display (str i)))))))
	      str
	      ))))

    (define s7_write write)
    (define write
      (lambda (obj . args)
	(if (string? obj)
	    (apply display (cons (object->string obj) args))
	    (apply s7_write (cons obj args)))))



    (set! (*s7* 'print-length) 1024)

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

	   (scheme-expressions (load "../../src/scheme.scm"))

	   (input-file (car source-files))
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




    ))

(let* ((seeds (list '() '()))
       (result (list (args-fold (cdr (command-line) )
				(map car option-table)
				unrecognized-processor
				operand-processor
				(apply values seeds)
				)))
       (options (car result))
       (source-files (reverse (cadr result)))

       (option-selected? (lambda (name selected-options)
			   (call/cc (lambda (return)
				      (for-each (lambda (selected-option)
						  (if (member name (option-names (car selected-option))) (return selected-option) ))
						selected-options)
				      #f))))

       (compile-selected (option-selected? "compile" options))
       
       )

  (cond (compile-selected (compile-program source-files))
	)
  
  )




(newline)
