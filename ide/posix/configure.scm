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

(define applications-directory "../../src/applications/")
(define modules-directory "../../src/modules/")
(define manifest-file "MANIFEST.s")

(define modules (list-sort string<? (directory-files modules-directory)))
(define applications (list-sort string<? (directory-files applications-directory)))

(define get-configuration
  (lambda (src-dir)
    (parameterize ((current-directory src-dir))
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
					      (string-append src-dir file))
					    (directory-files))))

		    `((requirements: . ,requirements)
		      (dependencies: . ,dependencies))))))



(define module-configurations
  (map (lambda (module)
	 (cons module
	       (get-configuration (string-append modules-directory module "/"))))
       modules))


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


(define application-configurations
  (map (lambda (application)
	 (cons application
	       (get-configuration (string-append applications-directory application "/" ))))
       applications))




(define autoscheme-imported-modules (reverse (cdr (link-modules (list (assoc "autoscheme" application-configurations))))))
(define autoscheme-mod-imported-modules (reverse (cdr (link-modules (list (assoc "autoscheme-mod" application-configurations))))))


(define variables (string-append "prefix = /usr/local\n"
				 "state_prefix = /var/local/lib\n"
				 "state_dir = autoscheme.state\n"
				 "\n"
				 "project_dir = ../..\n"
				 "src_dir = $(project_dir)/src\n"
				 "applications_dir = $(src_dir)/applications\n"
				 "library_dir = $(src_dir)/library\n"
				 "modules_dir = $(src_dir)/modules\n"
				 "prime_dir = $(src_dir)/prime\n"
				 "\n"
				 "gen_dir = gen\n"
				 "obj_dir = _obj\n"
				 "\n"
				 "bin_dir = bin\n"
				 "lib_dir = lib\n"
				 "libexec_dir = libexec\n"
				 "\n"
				 "compile_options = -fbracket-depth=10000\n"
				 "strict_options = -Wall -Wextra -pedantic -Wmissing-prototypes -Wstrict-prototypes -Wold-style-definition $(compile_options)\n"
				 "strict_options_89 = $(strict_options) -std=c89\n"
				 "strict_options_99 = $(strict_options) -std=c99\n"
				 "\n"
				 "autoscheme_requirements = \\\n\t"				
				 (string-join autoscheme-imported-modules " \\\n\t") "\n"
				 "\n"
				 "autoscheme_mod_requirements = \\\n\t"				
				 (string-join autoscheme-mod-imported-modules " \\\n\t") "\n"
				 "\n"
				 "module_objects = \\\n\t"
				 (string-join (map (lambda (module)
						     (string-append "$(obj_dir)/" module ".o"))
						   linked-modules)
					      " \\\n\t") 
				 "\n"
				 "\n"
				 "export DESTDIR prefix state_prefix\n"
				 "\n"
				 ))

(define targets
  (string-append "all: build\n"
		 "\n"
		 "build: $(gen_dir)/Makefile\n"
		 "	$(MAKE) -f $(gen_dir)/Makefile $(lib_dir)/libautoscheme.a \n"
		 "	$(MAKE) -f $(gen_dir)/Makefile $(bin_dir)/autoscheme\n"
		 "	$(MAKE) -f $(gen_dir)/Makefile $(bin_dir)/autoscheme-mod\n"
		 "\n"
		 "install: \n"
		 "	mkdir -p $(DESTDIR)$(prefix)/$(bin_dir)\n"
		 "	cp $(bin_dir)/autoscheme $(DESTDIR)$(prefix)/$(bin_dir)/\n"
		 "	mkdir -p $(DESTDIR)$(prefix)/$(libexec_dir)\n"
		 "	cp $(libexec_dir)/autoscheme-prime $(DESTDIR)$(prefix)/$(libexec_dir)/\n"
		 "	mkdir -p $(DESTDIR)$(prefix)/$(lib_dir)\n"
		 "	cp $(lib_dir)/libautoscheme.a $(DESTDIR)$(prefix)/$(lib_dir)/\n"
		 "\n"
		 "	mkdir -p $(DESTDIR)$(state_prefix)/$(state_dir)\n"
		 "	mkdir -p $(DESTDIR)$(state_prefix)/$(state_dir)/src\n"
		 "	mkdir -p $(DESTDIR)$(state_prefix)/$(state_dir)/rep\n"
		 "	mkdir -p $(DESTDIR)$(state_prefix)/$(state_dir)/ide/posix\n"
		 "\n"
		 "	cp -R . $(DESTDIR)$(state_prefix)/$(state_dir)/ide/posix\n"
		 "	cp -R $(src_dir)/applications $(DESTDIR)$(state_prefix)/$(state_dir)/src/\n"
		 "\n"
		 "uninstall: \n"
		 "	rm -f $(DESTDIR)$(prefix)/$(bin_dir)/autoscheme\n"
		 "	rm -f $(DESTDIR)$(prefix)/$(libexec_dir)/autoscheme-prime\n"
		 "	rm -f $(DESTDIR)$(prefix)/$(lib_dir)/libautoscheme.a\n"
		 "	rm -rf $(DESTDIR)$(state_prefix)/$(state_dir)\n"
		 "\n"
		 "clean:\n"
		 "	rm -rf $(gen_dir)\n"
		 "	rm -rf $(obj_dir)\n"
		 "	rm -rf $(bin_dir)\n"
		 "	rm -rf $(lib_dir)\n"
		 "	rm -rf $(libexec_dir)\n"
		 "\n"
		 "#####################\n"
		 "\n"
		 "$(gen_dir)/Makefile: $(libexec_dir)/autoscheme-prime\n"
		 "	mkdir -p $(gen_dir)\n"
		 "	$(libexec_dir)/autoscheme-prime -i configure.scm\n"
		 "\n"
		 "$(libexec_dir)/autoscheme-prime: \n"
		 "	mkdir -p $(libexec_dir)\n"
		 "	$(CC) $(strict_options_99) -o $(libexec_dir)/autoscheme-prime -I$(src_dir)/library -lm $(src_dir)/library/bignum.c $(src_dir)/library/autoscheme.c \\\n"
		 "	$(prime_dir)/autoscheme.c \\\n"
		 "	$(prime_dir)/load_modules.c \\\n"
		 (string-join (map (lambda (module)
				     (string-append "\t$(prime_dir)/" module ".c"))
				   linked-modules) 
			      " \\\n") 
		 "\n"
		 "\n"
		 "#####################\n"
		 "\n"		 
		 "$(bin_dir)/autoscheme: $(libexec_dir)/autoscheme-prime $(lib_dir)/libautoscheme.a \\\n\t"
		 (string-join (cdr (assoc 'dependencies: (cdr (assoc "autoscheme" application-configurations)))) " \\\n\t") "\n"
		 "\n"
		 "	mkdir -p $(bin_dir)\n"
		 "	$(libexec_dir)/autoscheme-prime --compile=$(applications_dir)/autoscheme/main.scm -o $(gen_dir)/autoscheme.c --load-modules=\"$(autoscheme_requirements)\"\n"
		 "	$(CC) $(strict_options_89) -o $(bin_dir)/autoscheme $(gen_dir)/autoscheme.c -lm -lautoscheme -L$(lib_dir) -I$(library_dir)\n"
		 "\n"
		 "$(bin_dir)/autoscheme-mod: $(libexec_dir)/autoscheme-prime $(lib_dir)/libautoscheme.a \\\n\t"
		 (string-join (cdr (assoc 'dependencies: (cdr (assoc "autoscheme-mod" application-configurations)))) " \\\n\t") "\n"
		 "\n"
		 "	mkdir -p $(bin_dir)\n"
		 "	$(libexec_dir)/autoscheme-prime --compile=$(applications_dir)/autoscheme-mod/main.scm -o $(gen_dir)/autoscheme-mod.c --load-modules=\"$(autoscheme_mod_requirements)\"\n"
		 "	$(CC) $(strict_options_89) -o $(bin_dir)/autoscheme-mod $(gen_dir)/autoscheme-mod.c -lautoscheme -L$(lib_dir) -I$(library_dir)\n"
		 "\n"
		 "$(lib_dir)/libautoscheme.a: $(obj_dir)/bignum.o $(obj_dir)/libautoscheme.o $(obj_dir)/load_modules.o $(module_objects)\n"
		 "	mkdir -p $(lib_dir)\n"
		 "	ar cr $(lib_dir)/libautoscheme.a $(obj_dir)/libautoscheme.o $(obj_dir)/bignum.o $(obj_dir)/load_modules.o $(module_objects)\n"
		 "\n"
		 "$(obj_dir)/load_modules.o: $(gen_dir)/load_modules.c\n"
		 "	$(CC) $(strict_options_89) -c $(gen_dir)/load_modules.c -o $(obj_dir)/load_modules.o -I$(library_dir)\n"
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
					     (string-append "$(obj_dir)/" module ".o: $(libexec_dir)/autoscheme-prime \\\n\t" 
							    (string-join (cdr (assoc 'dependencies: (cdr (assoc module module-configurations)))) " \\\n\t") "\n"
							    "\n"
							    "	$(libexec_dir)/autoscheme-prime --compile-module=$(modules_dir)/" module "/module.scm -n " module " -o $(gen_dir)/" module ".c\n"
							    "	$(CC) $(strict_options_89) -c $(gen_dir)/" module ".c -o $(obj_dir)/" module ".o -I$(library_dir)\n"
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