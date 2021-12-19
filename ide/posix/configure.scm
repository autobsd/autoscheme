;;  This file is part of the 'AutoScheme' project.
;;  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
;;  SPDX-License-Identifier: BSD-2-Clause

(import (auto scheme base)
	(auto scheme write)
	(auto scheme directory)
	(auto scheme path)
	(auto scheme list)
	(auto scheme sort)
	(auto scheme string)
	(auto scheme args fold)
	(auto scheme args)

	(scheme file)
	(scheme process-context)
	(scheme read)
	(auto scheme compile)
	)

(display "configuring build...")(newline)
(define applications-directory "../../src/applications/")
(define modules-directory "../../src/modules/")
(define manifest-file "MANIFEST.s")

(define prefix "/usr/local")
(define state-prefix "/var/local/lib")
(define install-path "$(DESTDIR)$(prefix)")


(define option-table (quasiquote ((,(option '(#\p "prefix") #t #f (lambda (option name arg . seeds) (set! prefix arg) (apply values seeds))) "Set installation prefix" "PREFIX")
				  (,(option '(#\s "state-prefix") #t #f (lambda (option name arg . seeds) (set! state-prefix arg) (apply values seeds))) "Set state prefix" "PREFIX")
				  (,(option '("install-path") #t #f (lambda (option name arg . seeds) (set! install-path arg) (apply values seeds))) "Set installation path" "PATH")
				  (,(option '(#\h "help") #f #f (lambda args (display-version) (display-usage) (exit))) "Show this message"))))

(define display-usage
  (lambda args
    (apply display (cons "Usage: configure.scm [option ...]" args))(apply newline args)
    (apply args-usage (cons option-table args))))   

(define operand-processor 
  (lambda (operand . seeds)
    (display (string-append "configure.scm: unrecognized operand '" operand "'") (current-error-port))(newline (current-error-port))
    (display-usage (current-error-port))
    (exit 1)))

(define unrecognized-processor 
  (lambda (option name arg . seeds)
    (let ((name-string (if (char? name) (string-append "-" (string name)) (string-append "--" name))))
      (display (string-append "configure.scm: unrecognized option " name-string) (current-error-port))(newline (current-error-port))
      (display-usage (current-error-port))
      (exit 1))))

(args-fold (cdr (command-line)) (map car option-table) unrecognized-processor operand-processor '() '())



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
					    (directory-files (current-directory)))))

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


(define variables (string-append "prefix = " prefix "\n"
				 "install_path = " install-path "\n"
				 "state_prefix = " state-prefix "\n"
				 "state_dir = autoscheme.state\n"
				 "state_path = $(DESTDIR)$(state_prefix)/$(state_dir)\n"
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
				 "#####################\n"
				 "\n"

				 ))

(define targets
  (string-append "all: build\n"
		 "\n"
		 "build: $(libexec_dir)/autoscheme-prime $(gen_dir)/Makefile\n"
		 "	$(MAKE) -f $(gen_dir)/Makefile build_targets \n"
		 "\n"
		 "install: $(gen_dir)/Makefile\n"
		 "	$(MAKE) -f $(gen_dir)/Makefile install_targets \n"
		 "\n"
		 "uninstall: $(gen_dir)/Makefile\n"
		 "	$(MAKE) -f $(gen_dir)/Makefile uninstall_targets \n"
		 "\n"
		 "clean:\n"
		 "	rm -rf $(gen_dir)\n"
		 "	rm -rf $(obj_dir)\n"
		 "	rm -rf $(bin_dir)\n"
		 "	rm -rf $(lib_dir)\n"
		 "	rm -rf $(libexec_dir)\n"
		 "\n"
		 "\n"
		 "#####################\n"
		 "\n"
		 "$(gen_dir)/Makefile: configure.scm\n"
		 "	mkdir -p $(gen_dir)\n"
		 "	$(libexec_dir)/autoscheme-prime -i configure.scm --prefix=\"$(prefix)\" --state-prefix=\"$(state_prefix)\"\n"
		 "\n"
		 "$(libexec_dir)/autoscheme-prime: \n"
		 "	mkdir -p $(libexec_dir)\n"
		 "	$(CC) $(strict_options_99) -o $(libexec_dir)/autoscheme-prime -I$(src_dir)/library -lm $(src_dir)/library/error.c $(src_dir)/library/bignum.c $(src_dir)/library/autoscheme.c \\\n"
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
		 "build_targets: $(lib_dir)/libautoscheme.a $(bin_dir)/autoscheme $(bin_dir)/autoscheme-mod\n"
		 "\n"
		 "install_targets: \n"
		 "	mkdir -p $(install_path)/$(bin_dir)\n"
		 "	cp $(bin_dir)/autoscheme $(install_path)/$(bin_dir)/\n"
		 "	mkdir -p $(install_path)/$(libexec_dir)\n"
		 "	cp $(libexec_dir)/autoscheme-prime $(install_path)/$(libexec_dir)/\n"
		 "	mkdir -p $(install_path)/$(lib_dir)\n"
		 "	cp $(lib_dir)/libautoscheme.a $(install_path)/$(lib_dir)/\n"
		 "\n"
		 "	mkdir -p $(state_path)\n"
		 "	mkdir -p $(state_path)/src\n"
		 "	mkdir -p $(state_path)/rep/autobsd/autoscheme\n"
		 "	mkdir -p $(state_path)/rep/autoscheme-modules\n"
		 "	mkdir -p $(state_path)/ide/posix\n"
		 "\n"
		 ;; "	cp -R gen/Makefile $(state_path)/ide/posix/\n"
		 "	cp -R gen/lock.s $(state_path)/ide/posix/\n"
		 "	cp -R configure.scm $(state_path)/ide/posix/\n"
		 "	cp -R $(src_dir)/applications $(state_path)/src/\n"
		 "	cp -R $(src_dir)/library $(state_path)/src/\n"
		 "	cp -R $(src_dir)/modules $(state_path)/src/\n"
		 "\n"
		 "uninstall_targets: \n"
		 "	rm -f $(install_path)/$(bin_dir)/autoscheme\n"
		 "	rm -f $(install_path)/$(libexec_dir)/autoscheme-prime\n"
		 "	rm -f $(install_path)/$(lib_dir)/libautoscheme.a\n"
		 "	rm -rf $(state_path)\n"
		 "\n"
		 "#####################\n"
		 "\n"		 
		 "$(bin_dir)/autoscheme: $(lib_dir)/libautoscheme.a \\\n\t"
		 (string-join (cdr (assoc 'dependencies: (cdr (assoc "autoscheme" application-configurations)))) " \\\n\t") "\n"
		 "\n"
		 "	mkdir -p $(bin_dir)\n"
		 "	$(libexec_dir)/autoscheme-prime --compile=$(applications_dir)/autoscheme/main.scm -o $(gen_dir)/autoscheme.c --load-modules=\"$(autoscheme_requirements)\"\n"
		 "	$(CC) $(strict_options_89) -o $(bin_dir)/autoscheme $(gen_dir)/autoscheme.c -lm -lautoscheme -L$(lib_dir) -I$(library_dir)\n"
		 "\n"
		 "$(bin_dir)/autoscheme-mod: $(lib_dir)/libautoscheme.a \\\n\t"
		 (string-join (cdr (assoc 'dependencies: (cdr (assoc "autoscheme-mod" application-configurations)))) " \\\n\t") "\n"
		 "\n"
		 "	mkdir -p $(bin_dir)\n"
		 "	$(libexec_dir)/autoscheme-prime --compile=$(applications_dir)/autoscheme-mod/main.scm -o $(gen_dir)/autoscheme-mod.c --load-modules=\"$(autoscheme_mod_requirements)\"\n"
		 "	$(CC) $(strict_options_89) -o $(bin_dir)/autoscheme-mod $(gen_dir)/autoscheme-mod.c -lautoscheme -L$(lib_dir) -I$(library_dir) -DINSTALL_PATH=$(install_path) -DSTATE_PATH=$(state_path)\n"
		 "\n"
		 "$(lib_dir)/libautoscheme.a: $(obj_dir)/error.o $(obj_dir)/bignum.o $(obj_dir)/libautoscheme.o $(obj_dir)/load_modules.o $(module_objects)\n"
		 "	mkdir -p $(lib_dir)\n"
		 "	ar cr $(lib_dir)/libautoscheme.a $(obj_dir)/libautoscheme.o $(obj_dir)/bignum.o $(obj_dir)/error.o $(obj_dir)/load_modules.o $(module_objects)\n"
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
		 "$(obj_dir)/error.o: $(src_dir)/library/error.c $(src_dir)/library/error.h\n"
		 "	mkdir -p $(obj_dir)\n"
		 "	$(CC) $(strict_options_99) -c $(src_dir)/library/error.c -o $(obj_dir)/error.o\n"
		 "\n"
		 "#####################\n"
		 "\n"
		 (apply string-append (map (lambda (module)
					     (string-append "$(obj_dir)/" module ".o:\\\n\t" 
							    (string-join (cdr (assoc 'dependencies: (cdr (assoc module module-configurations)))) " \\\n\t") "\n"
							    "\n"
							    "	$(libexec_dir)/autoscheme-prime --compile-module=$(modules_dir)/" module "/module.scm -n " module " -o $(gen_dir)/" module ".c\n"
							    "	$(CC) $(strict_options_89) -c $(gen_dir)/" module ".c -o $(obj_dir)/" module ".o -I$(library_dir)\n"
							    "\n"))
					   modules))))


(create-directory "gen" #t)

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
								    (string-append "    foreign_function " (module-loader-name module) ";\n"))
								  lib-loaded-modules))
					"\n"
					(apply string-append (map (lambda (module)
								    (string-append "    " (module-loader-name module) "( environment );\n"))
								  lib-loaded-modules))
					"\n"
					"    return environment;\n"
					"}\n"
					))

(define output-file-port (open-output-file "gen/load_modules.c"))
(display load-modules-src output-file-port)
(close-output-port output-file-port)




(define output-file-port (open-output-file "gen/lock.s"))
(write modules output-file-port)(newline output-file-port)
(close-output-port output-file-port)