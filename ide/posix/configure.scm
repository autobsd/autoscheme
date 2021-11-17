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
	(scheme process-context)
	(scheme read)
	)

(display "configuring build...")(newline)

(define application-directory "../../src/application/autoscheme")
(define modules-directory "../../src/modules")
(define manifest-file "MANIFEST.s")

(define modules (list-sort string<? (directory-files modules-directory)))

(define get-configuration
  (lambda ()
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
				(string-append (current-directory) file))
			      (directory-files))))

      `((requirements: . ,requirements)
	(dependencies: . ,dependencies)))))



(define module-configurations
  (parameterize ((current-directory modules-directory))
		(map (lambda (module)
		       (parameterize ((current-directory module)
				      )
				     
				     (cons module
					   (get-configuration)))
		       )
		     modules)))



(define link-modules 
  (lambda (configuration-list)
    (letrec ((_linked-modules '("scheme"))
	     (add-module (lambda (configuration)
			   (let ((module (car configuration)))
			     (cond ((not (member module _linked-modules))
				    (for-each (lambda (dep)
						(add-module (assoc dep module-configurations)))
					      (cdr (assoc 'requirements: (cdr configuration))))
				    (set! _linked-modules (cons module _linked-modules))))))))

      (for-each add-module configuration-list)
      _linked-modules)))


(define linked-modules (link-modules module-configurations))

(define lib-loaded-modules (reverse linked-modules))


(define application-configuration
  (parameterize ((current-directory application-directory)
		 )
		(cons "application" (get-configuration))))

(define app-loaded-modules (reverse (cdr (link-modules (list application-configuration)))))


(define variables (string-append "autoscheme_sources =  \\\n"
				 "	$(src_dir)/application/autoscheme/main.scm \\\n"
				 "\n"
				 "autoscheme_dependencies = $(autoscheme_sources)\n"
				 "\n"
				 "autoscheme_requirements = \\\n"				
				 (apply string-append (map (lambda (module)
							     (string-append "\t" module " \\\n"))
							   app-loaded-modules))
				 "\n"
				 "module_objects = \\\n"
				 (apply string-append (map (lambda (module)
							     (string-append "\t$(obj_dir)/" module ".o \\\n"))
							   linked-modules))
				 ))

(define targets
  (string-append "\n"
		 "all: build\n"
		 "\n"
		 "build: $(lib_dir)/libautoscheme.a $(bin_dir)/autoscheme  \n"
		 "\n"
		 "$(bin_dir)/autoscheme: $(autoscheme_dependencies)\n"
		 "	mkdir -p $(bin_dir)\n"
		 "	$(libexec_dir)/autoscheme-prime --compile $(autoscheme_sources) -o $(gen_dir)/autoscheme.c --load-modules=\"$(autoscheme_requirements)\"\n"
		 "	$(CC) $(strict_options_89) -o $(bin_dir)/autoscheme $(gen_dir)/autoscheme.c -lm -lautoscheme -L$(lib_dir) -I$(include_dir)\n"
		 "\n"
		 "$(lib_dir)/libautoscheme.a: $(obj_dir)/bignum.o $(obj_dir)/libautoscheme.o $(obj_dir)/load_modules.o $(module_objects)\n"
		 "	mkdir -p $(lib_dir)\n"
		 "	ar cr $(lib_dir)/libautoscheme.a $(obj_dir)/libautoscheme.o $(obj_dir)/bignum.o $(obj_dir)/load_modules.o $(module_objects)\n"
		 "\n"
		 "$(obj_dir)/load_modules.o: $(gen_dir)/load_modules.c\n"
		 "	$(CC) $(strict_options_89) -c $(gen_dir)/load_modules.c -o $(obj_dir)/load_modules.o -I$(include_dir)\n"
		 "\n"
		 "$(obj_dir)/libautoscheme.o: $(src_dir)/library/autoscheme.c $(src_dir)/library/autoscheme.h\n"
		 "	mkdir -p $(obj_dir)\n"
		 "	$(CC) $(strict_options_89) -c $(src_dir)/library/autoscheme.c -o $(obj_dir)/libautoscheme.o\n"
		 "\n"
		 "$(obj_dir)/bignum.o: $(src_dir)/library/bignum.c $(src_dir)/library/bignum.h\n"
		 "	mkdir -p $(obj_dir)\n"
		 "	$(CC) $(strict_options_99) -c $(src_dir)/library/bignum.c -o $(obj_dir)/bignum.o\n"
		 "\n"
		 "#####################\n"
		 "\n"
		 (apply string-append (map (lambda (module)
					     (string-append "$(obj_dir)/" module ".o:  \\\n\t" 
							    (string-join (cdr (assoc 'dependencies: (cdr (assoc module module-configurations)))) " \\\n\t") "\n"
							    "\n"
							    "	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/" module "/module.scm -n " module " -o $(gen_dir)/" module ".c\n"
							    "	$(CC) $(strict_options_89) -c $(gen_dir)/" module ".c -o $(obj_dir)/" module ".o -I$(include_dir)\n"
							    "\n"))
					   modules))))

(define output-file-port (open-output-file "gen/Makefile"))

(display variables output-file-port)(newline output-file-port)
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
								  lib-loaded-modules))
					"\n"
					(apply string-append (map (lambda (module)
								    (string-append "    LOAD_MODULE__" module "( environment );\n"))
								  lib-loaded-modules))
					"\n"
					"    return environment;\n"
					"}\n"
					))

(define output-file-port (open-output-file "gen/load_modules.c"))
(display load-modules-src output-file-port)
(close-output-port output-file-port)