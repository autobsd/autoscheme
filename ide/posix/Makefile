#  This file is part of the 'AutoScheme' project.
#  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
#  SPDX-License-Identifier: BSD-2-Clause

prefix = /usr/local
state_prefix = /var/local/lib
state_dir = autoscheme.state

project_dir = ../..
src_dir = $(project_dir)/src
applications_dir = $(src_dir)/applications
library_dir = $(src_dir)/library
modules_dir = $(src_dir)/modules
prime_dir = $(src_dir)/prime

gen_dir = gen
obj_dir = _obj

bin_dir = bin
lib_dir = lib
libexec_dir = libexec

compile_options = -fbracket-depth=10000
strict_options = -Wall -Wextra -pedantic -Wmissing-prototypes -Wstrict-prototypes -Wold-style-definition $(compile_options)
strict_options_89 = $(strict_options) -std=c89
strict_options_99 = $(strict_options) -std=c99

autoscheme_requirements = \
	scheme \
	auto_scheme_base \
	auto_scheme_write \
	auto_scheme_args_fold \
	auto_scheme_char_set \
	auto_scheme_string \
	scheme_cxr \
	auto_scheme_args \
	auto_scheme_char \
	auto_scheme_path \
	auto_scheme_file \
	scheme_read \
	auto_scheme_compile \
	auto_scheme_environment \
	scheme_eval \
	scheme_load \
	auto_scheme_interpret \
	scheme_process_context

autoscheme_mod_requirements = \
	scheme \
	auto_scheme_base \
	auto_scheme_write \
	auto_scheme_args_fold \
	auto_scheme_char_set \
	auto_scheme_string \
	scheme_cxr \
	auto_scheme_args \
	scheme_process_context

module_objects = \
	$(obj_dir)/scheme_repl.o \
	$(obj_dir)/scheme_process_context.o \
	$(obj_dir)/auto_scheme_sort.o \
	$(obj_dir)/auto_scheme_process.o \
	$(obj_dir)/auto_scheme_port.o \
	$(obj_dir)/auto_scheme_memory.o \
	$(obj_dir)/auto_scheme_macro.o \
	$(obj_dir)/auto_scheme_list.o \
	$(obj_dir)/auto_scheme_lazy.o \
	$(obj_dir)/auto_scheme_interpret.o \
	$(obj_dir)/scheme_load.o \
	$(obj_dir)/scheme_eval.o \
	$(obj_dir)/auto_scheme_inexact.o \
	$(obj_dir)/auto_scheme_environment.o \
	$(obj_dir)/auto_scheme_directory.o \
	$(obj_dir)/auto_scheme_compile.o \
	$(obj_dir)/scheme_read.o \
	$(obj_dir)/auto_scheme_file.o \
	$(obj_dir)/auto_scheme_path.o \
	$(obj_dir)/auto_scheme_closure.o \
	$(obj_dir)/auto_scheme_char.o \
	$(obj_dir)/auto_scheme_args.o \
	$(obj_dir)/scheme_cxr.o \
	$(obj_dir)/auto_scheme_string.o \
	$(obj_dir)/auto_scheme_char_set.o \
	$(obj_dir)/auto_scheme_args_fold.o \
	$(obj_dir)/auto_scheme_write.o \
	$(obj_dir)/auto_scheme_base.o \
	$(obj_dir)/scheme.o

export DESTDIR prefix state_prefix


all: build

build: $(gen_dir)/Makefile
	$(MAKE) -f $(gen_dir)/Makefile $(lib_dir)/libautoscheme.a 
	$(MAKE) -f $(gen_dir)/Makefile $(bin_dir)/autoscheme
	$(MAKE) -f $(gen_dir)/Makefile $(bin_dir)/autoscheme-mod

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
	rm -rf $(lib_dir)
	rm -rf $(libexec_dir)

#####################

$(gen_dir)/Makefile: $(libexec_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(libexec_dir)/autoscheme-prime -i configure.scm

$(libexec_dir)/autoscheme-prime: 
	mkdir -p $(libexec_dir)
	$(CC) $(strict_options_99) -o $(libexec_dir)/autoscheme-prime -I$(src_dir)/library -lm $(src_dir)/library/bignum.c $(src_dir)/library/autoscheme.c \
	$(prime_dir)/autoscheme.c \
	$(prime_dir)/load_modules.c \
	$(prime_dir)/scheme_repl.c \
	$(prime_dir)/scheme_process_context.c \
	$(prime_dir)/auto_scheme_sort.c \
	$(prime_dir)/auto_scheme_process.c \
	$(prime_dir)/auto_scheme_port.c \
	$(prime_dir)/auto_scheme_memory.c \
	$(prime_dir)/auto_scheme_macro.c \
	$(prime_dir)/auto_scheme_list.c \
	$(prime_dir)/auto_scheme_lazy.c \
	$(prime_dir)/auto_scheme_interpret.c \
	$(prime_dir)/scheme_load.c \
	$(prime_dir)/scheme_eval.c \
	$(prime_dir)/auto_scheme_inexact.c \
	$(prime_dir)/auto_scheme_environment.c \
	$(prime_dir)/auto_scheme_directory.c \
	$(prime_dir)/auto_scheme_compile.c \
	$(prime_dir)/scheme_read.c \
	$(prime_dir)/auto_scheme_file.c \
	$(prime_dir)/auto_scheme_path.c \
	$(prime_dir)/auto_scheme_closure.c \
	$(prime_dir)/auto_scheme_char.c \
	$(prime_dir)/auto_scheme_args.c \
	$(prime_dir)/scheme_cxr.c \
	$(prime_dir)/auto_scheme_string.c \
	$(prime_dir)/auto_scheme_char_set.c \
	$(prime_dir)/auto_scheme_args_fold.c \
	$(prime_dir)/auto_scheme_write.c \
	$(prime_dir)/auto_scheme_base.c \
	$(prime_dir)/scheme.c

#####################

$(bin_dir)/autoscheme: $(libexec_dir)/autoscheme-prime $(lib_dir)/libautoscheme.a \
	../../src/applications/autoscheme/main.scm \
	../../src/applications/autoscheme/MANIFEST.s

	mkdir -p $(bin_dir)
	$(libexec_dir)/autoscheme-prime --compile $(applications_dir)/autoscheme/main.scm -o $(gen_dir)/autoscheme.c --load-modules="$(autoscheme_requirements)"
	$(CC) $(strict_options_89) -o $(bin_dir)/autoscheme $(gen_dir)/autoscheme.c -lm -lautoscheme -L$(lib_dir) -I$(library_dir)

$(bin_dir)/autoscheme-mod: $(libexec_dir)/autoscheme-prime $(lib_dir)/libautoscheme.a \
	../../src/applications/autoscheme-mod/main.scm \
	../../src/applications/autoscheme-mod/MANIFEST.s

	mkdir -p $(bin_dir)
	$(libexec_dir)/autoscheme-prime --compile $(applications_dir)/autoscheme-mod/main.scm -o $(gen_dir)/autoscheme-mod.c --load-modules="$(autoscheme_mod_requirements)"
	$(CC) $(strict_options_89) -o $(bin_dir)/autoscheme-mod $(gen_dir)/autoscheme-mod.c -lautoscheme -L$(lib_dir) -I$(library_dir)

$(lib_dir)/libautoscheme.a: $(obj_dir)/bignum.o $(obj_dir)/libautoscheme.o $(obj_dir)/load_modules.o $(module_objects)
	mkdir -p $(lib_dir)
	ar cr $(lib_dir)/libautoscheme.a $(obj_dir)/libautoscheme.o $(obj_dir)/bignum.o $(obj_dir)/load_modules.o $(module_objects)

$(obj_dir)/load_modules.o: $(gen_dir)/load_modules.c
	$(CC) $(strict_options_89) -c $(gen_dir)/load_modules.c -o $(obj_dir)/load_modules.o -I$(library_dir)

$(obj_dir)/libautoscheme.o: $(src_dir)/library/autoscheme.c $(src_dir)/library/autoscheme.h
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(src_dir)/library/autoscheme.c -o $(obj_dir)/libautoscheme.o

$(obj_dir)/bignum.o: $(src_dir)/library/bignum.c $(src_dir)/library/bignum.h
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_99) -c $(src_dir)/library/bignum.c -o $(obj_dir)/bignum.o

#####################

$(obj_dir)/auto_scheme_args.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_args/manifest.s \
	../../src/modules/auto_scheme_args/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_args/module.scm -n auto_scheme_args -o $(gen_dir)/auto_scheme_args.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_args.c -o $(obj_dir)/auto_scheme_args.o -I$(library_dir)

$(obj_dir)/auto_scheme_args_fold.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_args_fold/manifest.s \
	../../src/modules/auto_scheme_args_fold/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_args_fold/module.scm -n auto_scheme_args_fold -o $(gen_dir)/auto_scheme_args_fold.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_args_fold.c -o $(obj_dir)/auto_scheme_args_fold.o -I$(library_dir)

$(obj_dir)/auto_scheme_base.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_base/definitions.c \
	../../src/modules/auto_scheme_base/declarations.h \
	../../src/modules/auto_scheme_base/module.scm \
	../../src/modules/auto_scheme_base/initialization.c

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_base/module.scm -n auto_scheme_base -o $(gen_dir)/auto_scheme_base.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_base.c -o $(obj_dir)/auto_scheme_base.o -I$(library_dir)

$(obj_dir)/auto_scheme_char.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_char/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_char/module.scm -n auto_scheme_char -o $(gen_dir)/auto_scheme_char.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_char.c -o $(obj_dir)/auto_scheme_char.o -I$(library_dir)

$(obj_dir)/auto_scheme_char_set.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_char_set/MANIFEST.s \
	../../src/modules/auto_scheme_char_set/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_char_set/module.scm -n auto_scheme_char_set -o $(gen_dir)/auto_scheme_char_set.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_char_set.c -o $(obj_dir)/auto_scheme_char_set.o -I$(library_dir)

$(obj_dir)/auto_scheme_closure.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_closure/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_closure/module.scm -n auto_scheme_closure -o $(gen_dir)/auto_scheme_closure.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_closure.c -o $(obj_dir)/auto_scheme_closure.o -I$(library_dir)

$(obj_dir)/auto_scheme_compile.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_compile/manifest.s \
	../../src/modules/auto_scheme_compile/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_compile/module.scm -n auto_scheme_compile -o $(gen_dir)/auto_scheme_compile.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_compile.c -o $(obj_dir)/auto_scheme_compile.o -I$(library_dir)

$(obj_dir)/auto_scheme_directory.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_directory/definitions.c \
	../../src/modules/auto_scheme_directory/declarations.h \
	../../src/modules/auto_scheme_directory/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_directory/module.scm -n auto_scheme_directory -o $(gen_dir)/auto_scheme_directory.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_directory.c -o $(obj_dir)/auto_scheme_directory.o -I$(library_dir)

$(obj_dir)/auto_scheme_environment.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_environment/definitions.c \
	../../src/modules/auto_scheme_environment/declarations.h \
	../../src/modules/auto_scheme_environment/module.scm \
	../../src/modules/auto_scheme_environment/initialization.c

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_environment/module.scm -n auto_scheme_environment -o $(gen_dir)/auto_scheme_environment.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_environment.c -o $(obj_dir)/auto_scheme_environment.o -I$(library_dir)

$(obj_dir)/auto_scheme_file.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_file/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_file/module.scm -n auto_scheme_file -o $(gen_dir)/auto_scheme_file.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_file.c -o $(obj_dir)/auto_scheme_file.o -I$(library_dir)

$(obj_dir)/auto_scheme_inexact.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_inexact/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_inexact/module.scm -n auto_scheme_inexact -o $(gen_dir)/auto_scheme_inexact.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_inexact.c -o $(obj_dir)/auto_scheme_inexact.o -I$(library_dir)

$(obj_dir)/auto_scheme_interpret.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_interpret/manifest.s \
	../../src/modules/auto_scheme_interpret/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_interpret/module.scm -n auto_scheme_interpret -o $(gen_dir)/auto_scheme_interpret.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_interpret.c -o $(obj_dir)/auto_scheme_interpret.o -I$(library_dir)

$(obj_dir)/auto_scheme_lazy.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_lazy/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_lazy/module.scm -n auto_scheme_lazy -o $(gen_dir)/auto_scheme_lazy.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_lazy.c -o $(obj_dir)/auto_scheme_lazy.o -I$(library_dir)

$(obj_dir)/auto_scheme_list.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_list/manifest.s \
	../../src/modules/auto_scheme_list/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_list/module.scm -n auto_scheme_list -o $(gen_dir)/auto_scheme_list.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_list.c -o $(obj_dir)/auto_scheme_list.o -I$(library_dir)

$(obj_dir)/auto_scheme_macro.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_macro/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_macro/module.scm -n auto_scheme_macro -o $(gen_dir)/auto_scheme_macro.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_macro.c -o $(obj_dir)/auto_scheme_macro.o -I$(library_dir)

$(obj_dir)/auto_scheme_memory.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_memory/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_memory/module.scm -n auto_scheme_memory -o $(gen_dir)/auto_scheme_memory.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_memory.c -o $(obj_dir)/auto_scheme_memory.o -I$(library_dir)

$(obj_dir)/auto_scheme_path.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_path/definitions.c \
	../../src/modules/auto_scheme_path/declarations.h \
	../../src/modules/auto_scheme_path/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_path/module.scm -n auto_scheme_path -o $(gen_dir)/auto_scheme_path.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_path.c -o $(obj_dir)/auto_scheme_path.o -I$(library_dir)

$(obj_dir)/auto_scheme_port.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_port/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_port/module.scm -n auto_scheme_port -o $(gen_dir)/auto_scheme_port.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_port.c -o $(obj_dir)/auto_scheme_port.o -I$(library_dir)

$(obj_dir)/auto_scheme_process.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_process/definitions.c \
	../../src/modules/auto_scheme_process/declarations.h \
	../../src/modules/auto_scheme_process/MANIFEST.s \
	../../src/modules/auto_scheme_process/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_process/module.scm -n auto_scheme_process -o $(gen_dir)/auto_scheme_process.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_process.c -o $(obj_dir)/auto_scheme_process.o -I$(library_dir)

$(obj_dir)/auto_scheme_sort.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_sort/manifest.s \
	../../src/modules/auto_scheme_sort/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_sort/module.scm -n auto_scheme_sort -o $(gen_dir)/auto_scheme_sort.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_sort.c -o $(obj_dir)/auto_scheme_sort.o -I$(library_dir)

$(obj_dir)/auto_scheme_string.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_string/manifest.s \
	../../src/modules/auto_scheme_string/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_string/module.scm -n auto_scheme_string -o $(gen_dir)/auto_scheme_string.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_string.c -o $(obj_dir)/auto_scheme_string.o -I$(library_dir)

$(obj_dir)/auto_scheme_write.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/auto_scheme_write/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/auto_scheme_write/module.scm -n auto_scheme_write -o $(gen_dir)/auto_scheme_write.c
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_write.c -o $(obj_dir)/auto_scheme_write.o -I$(library_dir)

$(obj_dir)/scheme.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/scheme/macros.scm \
	../../src/modules/scheme/finalization.c \
	../../src/modules/scheme/declarations.h \
	../../src/modules/scheme/module.scm \
	../../src/modules/scheme/procedures.scm \
	../../src/modules/scheme/initialization.c

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/scheme/module.scm -n scheme -o $(gen_dir)/scheme.c
	$(CC) $(strict_options_89) -c $(gen_dir)/scheme.c -o $(obj_dir)/scheme.o -I$(library_dir)

$(obj_dir)/scheme_cxr.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/scheme_cxr/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/scheme_cxr/module.scm -n scheme_cxr -o $(gen_dir)/scheme_cxr.c
	$(CC) $(strict_options_89) -c $(gen_dir)/scheme_cxr.c -o $(obj_dir)/scheme_cxr.o -I$(library_dir)

$(obj_dir)/scheme_eval.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/scheme_eval/manifest.s \
	../../src/modules/scheme_eval/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/scheme_eval/module.scm -n scheme_eval -o $(gen_dir)/scheme_eval.c
	$(CC) $(strict_options_89) -c $(gen_dir)/scheme_eval.c -o $(obj_dir)/scheme_eval.o -I$(library_dir)

$(obj_dir)/scheme_load.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/scheme_load/manifest.s \
	../../src/modules/scheme_load/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/scheme_load/module.scm -n scheme_load -o $(gen_dir)/scheme_load.c
	$(CC) $(strict_options_89) -c $(gen_dir)/scheme_load.c -o $(obj_dir)/scheme_load.o -I$(library_dir)

$(obj_dir)/scheme_process_context.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/scheme_process_context/definitions.c \
	../../src/modules/scheme_process_context/declarations.h \
	../../src/modules/scheme_process_context/manifest.s \
	../../src/modules/scheme_process_context/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/scheme_process_context/module.scm -n scheme_process_context -o $(gen_dir)/scheme_process_context.c
	$(CC) $(strict_options_89) -c $(gen_dir)/scheme_process_context.c -o $(obj_dir)/scheme_process_context.o -I$(library_dir)

$(obj_dir)/scheme_read.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/scheme_read/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/scheme_read/module.scm -n scheme_read -o $(gen_dir)/scheme_read.c
	$(CC) $(strict_options_89) -c $(gen_dir)/scheme_read.c -o $(obj_dir)/scheme_read.o -I$(library_dir)

$(obj_dir)/scheme_repl.o: $(libexec_dir)/autoscheme-prime \
	../../src/modules/scheme_repl/manifest.s \
	../../src/modules/scheme_repl/module.scm

	$(libexec_dir)/autoscheme-prime --compile-module $(modules_dir)/scheme_repl/module.scm -n scheme_repl -o $(gen_dir)/scheme_repl.c
	$(CC) $(strict_options_89) -c $(gen_dir)/scheme_repl.c -o $(obj_dir)/scheme_repl.o -I$(library_dir)


