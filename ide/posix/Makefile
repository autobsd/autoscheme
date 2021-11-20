#  This file is part of the 'AutoScheme' project.
#  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
#  SPDX-License-Identifier: BSD-2-Clause

prefix = /usr/local
state_prefix = /var/local/lib

project_dir = ../..
src_dir = $(project_dir)/src
prime_dir = $(src_dir)/prime

applications_dir = $(src_dir)/applications
modules_dir = $(src_dir)/modules
gen_dir = gen
bin_dir = bin
libexec_dir = libexec
obj_dir = _obj
lib_dir = lib
include_dir = include
state_dir = autoscheme.state


compile_options = -fbracket-depth=10000   
strict_options = -Wall -Wextra -pedantic -Wmissing-prototypes -Wstrict-prototypes -Wold-style-definition $(compile_options)
strict_options_89 = $(strict_options) -std=c89 
strict_options_99 = $(strict_options) -std=c99 




export

all: build

build: $(include_dir)/autoscheme.h $(gen_dir)/Makefile
	$(MAKE) -f $(gen_dir)/Makefile


$(gen_dir)/Makefile: $(libexec_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(libexec_dir)/autoscheme-prime -i configure.scm

$(include_dir)/autoscheme.h: $(src_dir)/library/autoscheme.h
	mkdir -p $(include_dir)	
	cp $(src_dir)/library/autoscheme.h $(include_dir)/autoscheme.h

#####################

$(libexec_dir)/autoscheme-prime: 
	mkdir -p $(libexec_dir)
	$(CC) $(strict_options_99) -o $(libexec_dir)/autoscheme-prime -I$(src_dir)/library -lm $(src_dir)/library/bignum.c $(src_dir)/library/autoscheme.c \
		$(prime_dir)/scheme.c \
		$(prime_dir)/auto_scheme_file.c \
		$(prime_dir)/auto_scheme_port.c \
		$(prime_dir)/auto_scheme_macro.c \
		$(prime_dir)/auto_scheme_base.c \
		$(prime_dir)/auto_scheme_write.c \
		$(prime_dir)/auto_scheme_char_set.c \
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
		$(prime_dir)/auto_scheme_process.c \
		$(prime_dir)/load_modules.c \
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

install: 
	mkdir -p $(DESTDIR)$(prefix)/$(bin_dir)
	cp $(bin_dir)/autoscheme $(DESTDIR)$(prefix)/$(bin_dir)/
	mkdir -p $(DESTDIR)$(prefix)/$(libexec_dir)
	cp $(libexec_dir)/autoscheme-prime $(DESTDIR)$(prefix)/$(libexec_dir)/
	mkdir -p $(DESTDIR)$(prefix)/$(lib_dir)
	cp $(lib_dir)/libautoscheme.a $(DESTDIR)$(prefix)/$(lib_dir)/

	mkdir -p $(DESTDIR)$(state_prefix)/$(state_dir)
	mkdir -p $(DESTDIR)$(state_prefix)/$(state_dir)/src
	mkdir -p $(DESTDIR)$(state_prefix)/$(state_dir)/rep
	mkdir -p $(DESTDIR)$(state_prefix)/$(state_dir)/ide/posix

	cp -R . $(DESTDIR)$(state_prefix)/$(state_dir)/ide/posix
	cp -R $(src_dir)/application $(DESTDIR)$(state_prefix)/$(state_dir)/src/



uninstall: 
	rm -f $(DESTDIR)$(prefix)/$(bin_dir)/autoscheme
	rm -f $(DESTDIR)$(prefix)/$(libexec_dir)/autoscheme-prime
	rm -f $(DESTDIR)$(prefix)/$(lib_dir)/libautoscheme.a
	rm -rf $(DESTDIR)$(state_prefix)/$(state_dir)

clean:
	rm -rf $(gen_dir)
	rm -rf $(obj_dir)
	rm -rf $(bin_dir)
	rm -rf $(include_dir)
	rm -rf $(lib_dir)
	rm -rf $(libexec_dir)

	rm -rf tmp
