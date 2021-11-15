;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (auto scheme base)
	(auto scheme write)
	(auto scheme file)
	(auto scheme directory)
	(auto scheme path)
	(auto scheme list)
	(auto scheme sort)
	(auto scheme string)
	(scheme read)
	)

(display "configuring build...")(newline)

(define modules-directory "../../src/modules")
(define manifest-file "MANIFEST.s")

(define modules (list-sort string<? (directory-files modules-directory)))

(define mod-alist
  (parameterize ((current-directory modules-directory))
		(map (lambda (module)
		       (parameterize ((current-directory module)
				      )
				     (let* ((alist (guard (condition ((read-error? condition) '())
								     (else (raise condition))
								     )
							  (with-input-from-file manifest-file (lambda ()
												(let ((alist (read)))
												  (cond ((eof-object? alist) '())
													((alist? alist) alist)
													(else (error "Configuration error - manifest is not a proper alist" 
														     (path-make-absolute manifest-file)))))))))
					    (requirements (cond ((assoc 'require: alist) => cdr)
								(else '())))
					    (dependencies (map (lambda (file)
								 (string-append "$(modules_dir)/" module "/" file))
							       (directory-files)))
					    (alist `((requirements: . ,requirements)
						     (dependencies: . ,dependencies))))
				       (cons module
					     alist))))
		     modules)))

(define dep-order 
  (letrec ((_dep-order '("scheme"))
	   (add-module (lambda (module)
			 (cond ((not (member module _dep-order))
				(display "checking requirements...\n")(write (assoc module mod-alist))(newline)(newline)
				(for-each (lambda (dep)
					    (add-module dep))
					  (cdr (assoc 'requirements: (cdr (assoc module mod-alist)))))
				(set! _dep-order (cons module _dep-order)))))))
    (for-each (lambda (module-dep)
		(add-module (car module-dep)))
	      mod-alist)
    _dep-order))

(define load-order (reverse dep-order))

(define dependencies 
  (let* ((_dependencies "")
	 (update-dependencies (lambda (module)
				(parameterize ((current-directory module))
					      (set! _dependencies (string-append _dependencies "\n" 
										 module "_dep = \\\n"))
					      (for-each (lambda (file)
							  (set! _dependencies (string-append _dependencies "\t$(modules_dir)/" 
											     module "/" file 
											     " \\\n")))
							(directory-files))))))
    (parameterize ((current-directory modules-directory))
		  (for-each (lambda (module)
			      (update-dependencies module))
			    modules))
    _dependencies))

(define mod-objs (string-append "module_objects = \\\n"
				(apply string-append (map (lambda (module)
							    (string-append "\t$(obj_dir)/" module ".o \\\n"))
							  dep-order))))

(define targets
  (string-append "\n"
		 "all: build\n"
		 "\n"
		 "build: $(lib_dir)/libautoscheme.a $(bin_dir)/autoscheme  \n"
		 "\n"
		 "$(bin_dir)/autoscheme: $(autoscheme_dependencies)\n"
		 "	$(bin_dir)/autoscheme-prime --compile $(autoscheme_sources) -o $(gen_dir)/autoscheme.c --load-modules=\"$(autoscheme_requirements)\"\n"
		 "	$(CC) $(strict_options_89) -o $(bin_dir)/autoscheme $(gen_dir)/autoscheme.c -lm -lautoscheme -L$(lib_dir) -I$(include_dir)\n"
		 "\n"
		 "$(lib_dir)/libautoscheme.a: $(obj_dir)/load_modules.o $(module_objects)\n"
		 "	ar cr $(lib_dir)/libautoscheme.a $(obj_dir)/libautoscheme.o $(obj_dir)/bignum.o $(obj_dir)/load_modules.o $(module_objects)\n"
		 "\n"
		 "$(obj_dir)/load_modules.o: $(gen_dir)/load_modules.c\n"
		 "	$(CC) $(strict_options_89) -c $(gen_dir)/load_modules.c -o $(obj_dir)/load_modules.o -I$(include_dir)\n"
		 "\n"
		 "#####################\n"
		 "\n"
		 (apply string-append (map (lambda (module)
					     (string-append "$(obj_dir)/" module ".o:  \\\n\t" 
							    (string-join (cdr (assoc 'dependencies: (cdr (assoc module mod-alist)))) " \\\n\t") "\n"
							    "\n"
							    "	$(bin_dir)/autoscheme-prime --compile-module $(modules_dir)/" module "/module.scm -n " module " -o $(gen_dir)/" module ".c\n"
							    "	$(CC) $(strict_options_89) -c $(gen_dir)/" module ".c -o $(obj_dir)/" module ".o -I$(include_dir)\n"
							    "\n"))
					   load-order))))

(define output-file-port (open-output-file "gen/Makefile"))

(display mod-objs output-file-port)(newline output-file-port)
(display targets output-file-port)(newline output-file-port)

(close-output-port output-file-port)


(define load-modules-src (string-append "#include \"autoscheme.h\"\n"
					"\n"
					"pointer ff_load_modules( pointer args );\n"
					"pointer ff_load_modules( pointer args )\n"
					"{\n"
					"    pointer environment = car( args );\n"
					"\n"
					(apply string-append (map (lambda (module)
								    (string-append "    foreign_function LOAD_MODULE__" module ";\n"))
								  load-order))
					"\n"
					(apply string-append (map (lambda (module)
								    (string-append "    LOAD_MODULE__" module "( environment );\n"))
								  load-order))
					"\n"
					"    return environment;\n"
					"}\n"
					))

(define output-file-port (open-output-file "gen/load_modules.c"))
(display load-modules-src output-file-port)
(close-output-port output-file-port)