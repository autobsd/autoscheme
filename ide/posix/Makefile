#  This file is part of the 'AutoScheme' project.
#  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
#  SPDX-License-Identifier: BSD-2-Clause

project_dir = ../..
src_dir = $(project_dir)/src
prime_dir = $(src_dir)/prime

modules_dir = $(src_dir)/modules
gen_dir = gen
bin_dir = bin
obj_dir = _obj
lib_dir = lib
include_dir = include
install_dir = /usr/local/bin
application_dir = /var/local/lib/autoscheme


compile_options = -fbracket-depth=10000   
strict_options = -Wall -Wextra -pedantic -Wmissing-prototypes -Wstrict-prototypes -Wold-style-definition $(compile_options)
strict_options_89 = $(strict_options) -std=c89 
strict_options_99 = $(strict_options) -std=c99 


autoscheme_sources =  \
	$(src_dir)/application/autoscheme.scm \

autoscheme_dependencies = $(autoscheme_sources)

autoscheme_requirements =  \
                 scheme \
                 auto_scheme_base \
                 auto_scheme_write \
                 auto_scheme_environment \
                 auto_scheme_args_fold \
                 auto_scheme_string \
                 scheme_cxr \
                 auto_scheme_args \
                 auto_scheme_char \
                 auto_scheme_closure \
                 auto_scheme_path \
                 auto_scheme_file \
                 scheme_read \
                 auto_scheme_compile \
                 auto_scheme_directory \
                 auto_scheme_inexact \
                 scheme_eval \
                 scheme_load \
                 auto_scheme_interpret \
                 auto_scheme_lazy \
                 auto_scheme_list \
                 auto_scheme_macro \
                 auto_scheme_memory \
                 auto_scheme_port \
                 auto_scheme_sort \
                 scheme_process_context \
                 scheme_repl \

export

all: build

build: $(obj_dir)/bignum.o $(obj_dir)/libautoscheme.o $(include_dir)/autoscheme.h $(gen_dir)/Makefile 
	mkdir -p $(lib_dir)
	mkdir -p $(bin_dir)
	$(MAKE) -f $(gen_dir)/Makefile

$(gen_dir)/Makefile: $(bin_dir)/autoscheme-prime
	echo "generating Makefile..."
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime -i configure.scm

$(include_dir)/autoscheme.h: $(src_dir)/library/autoscheme.h
	mkdir -p $(include_dir)	
	cp $(src_dir)/library/autoscheme.h $(include_dir)/autoscheme.h

$(obj_dir)/libautoscheme.o: $(src_dir)/library/autoscheme.c $(src_dir)/library/autoscheme.h
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(src_dir)/library/autoscheme.c -o $(obj_dir)/libautoscheme.o

$(obj_dir)/bignum.o: $(src_dir)/library/bignum.c $(src_dir)/library/bignum.h
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_99) -c $(src_dir)/library/bignum.c -o $(obj_dir)/bignum.o

#####################

$(bin_dir)/autoscheme-prime:
	mkdir -p $(bin_dir)
	$(CC) $(strict_options_99) -o $(bin_dir)/autoscheme-prime -I$(src_dir)/library -lm $(src_dir)/library/bignum.c $(src_dir)/library/autoscheme.c \
		$(prime_dir)/load_modules.c \
		$(prime_dir)/scheme.c \
		$(prime_dir)/auto_scheme_file.c \
		$(prime_dir)/auto_scheme_port.c \
		$(prime_dir)/auto_scheme_macro.c \
		$(prime_dir)/auto_scheme_base.c \
		$(prime_dir)/auto_scheme_write.c \
		$(prime_dir)/auto_scheme_string.c \
		$(prime_dir)/scheme_read.c \
		$(prime_dir)/scheme_process_context.c \
		$(prime_dir)/auto_scheme_directory.c \
		$(prime_dir)/auto_scheme_path.c \
		$(prime_dir)/auto_scheme_char.c \
		$(prime_dir)/auto_scheme_inexact.c \
		$(prime_dir)/auto_scheme_lazy.c \
		$(prime_dir)/auto_scheme_memory.c \
		$(prime_dir)/auto_scheme_closure.c \
		$(prime_dir)/auto_scheme_list.c \
		$(prime_dir)/auto_scheme_sort.c \
		$(prime_dir)/scheme_eval.c \
		$(prime_dir)/scheme_repl.c \
		$(prime_dir)/scheme_load.c \
		$(prime_dir)/auto_scheme_environment.c \
		$(prime_dir)/scheme_cxr.c \
		$(prime_dir)/auto_scheme_compile.c \
		$(prime_dir)/auto_scheme_interpret.c \
		$(prime_dir)/auto_scheme_args.c \
		$(prime_dir)/auto_scheme_args_fold.c \
		$(prime_dir)/autoscheme.c  

#####################

install: $(bin_dir)/autoscheme
	cp $(bin_dir)/autoscheme $(install_dir)/
	mkdir -p $(application_dir)
	cp -r .$(application_dir)/ $(application_dir)/ 

uninstall: 
	rm -f $(install_dir)/autoscheme
	rm -rf $(application_dir)

clean:
	rm -rf $(gen_dir)
	rm -rf $(obj_dir)
	rm -rf $(bin_dir)
	rm -rf $(include_dir)
	rm -rf $(lib_dir)

	rm -rf tmp
