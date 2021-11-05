#  This file is part of the 'AutoScheme' project.
#  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
#  SPDX-License-Identifier: BSD-2-Clause

project_dir = ../..
src_dir = $(project_dir)/src
prime_dir = $(src_dir)/prime

modules_dir = $(src_dir)/modules
gen_dir = $(project_dir)/gen
bin_dir = bin
obj_dir = _obj
lib_dir = lib
include_dir = include

install_dir = /usr/local/bin

compile_options = -fbracket-depth=10000   
strict_options = -Wall -Wextra -pedantic -Wmissing-prototypes -Wstrict-prototypes -Wold-style-definition $(compile_options)
strict_options_89 = $(strict_options) -std=c89 
strict_options_99 = $(strict_options) -std=c99 


autoscheme_sources =  \
	$(library_sources) \
	$(src_dir)/application/program.scm 



scheme_module_scm = $(modules_dir)/scheme/module.scm 
scheme_dep = \
	$(scheme_module_scm) \
	$(modules_dir)/scheme/procedures.scm \
	$(modules_dir)/scheme/macros.scm \
	$(modules_dir)/scheme/initialization.c \
	$(modules_dir)/scheme/finalization.c \
	$(modules_dir)/scheme/declarations.h \



auto_scheme_environment_module_scm = $(modules_dir)/auto/scheme/environment/module.scm 
auto_scheme_environment_dep = \
	$(auto_scheme_environment_module_scm) \
	$(modules_dir)/auto/scheme/environment/declarations.h \
	$(modules_dir)/auto/scheme/environment/definitions.c \
	$(modules_dir)/auto/scheme/environment/initialization.c \


auto_scheme_macro_module_scm = $(modules_dir)/auto/scheme/macro/module.scm 
auto_scheme_macro_dep = \
	$(auto_scheme_macro_module_scm) \

auto_scheme_base_module_scm = $(modules_dir)/auto/scheme/base/module.scm 
auto_scheme_base_dep = \
	$(auto_scheme_base_module_scm) \
	$(modules_dir)/auto/scheme/base/declarations.h \
	$(modules_dir)/auto/scheme/base/definitions.c \
	$(modules_dir)/auto/scheme/base/initialization.c \




scheme_eval_module_scm = $(modules_dir)/scheme/eval/module.scm 
scheme_eval_dep = \
	$(scheme_eval_module_scm) \
	$(modules_dir)/scheme/eval/declarations.h \
	$(modules_dir)/scheme/eval/definitions.c \

scheme_repl_module_scm = $(modules_dir)/scheme/repl/module.scm 
scheme_repl_dep = \
	$(scheme_repl_module_scm) \

scheme_load_module_scm = $(modules_dir)/scheme/load/module.scm 
scheme_load_dep = \
	$(scheme_load_module_scm) \


auto_scheme_list_module_scm = $(modules_dir)/auto/scheme/list/module.scm 
auto_scheme_list_dep = \
	$(auto_scheme_list_module_scm) \


auto_scheme_write_module_scm = $(modules_dir)/auto/scheme/write/module.scm 
auto_scheme_write_dep = \
	$(auto_scheme_write_module_scm) \

auto_scheme_args_fold_module_scm = $(modules_dir)/auto/scheme/args/fold/module.scm 
auto_scheme_args_fold_dep = \
	$(auto_scheme_args_fold_module_scm) \

auto_scheme_args_module_scm = $(modules_dir)/auto/scheme/args/module.scm 
auto_scheme_args_dep = \
	$(auto_scheme_args_module_scm) \

auto_scheme_string_module_scm = $(modules_dir)/auto/scheme/string/module.scm 
auto_scheme_string_dep = \
	$(auto_scheme_string_module_scm) \

scheme_cxr_module_scm = $(modules_dir)/scheme/cxr/module.scm 
scheme_cxr_dep = \
	$(scheme_cxr_module_scm) \

scheme_process_context_module_scm = $(modules_dir)/scheme/process/context/module.scm 
scheme_process_context_dep = \
	$(scheme_process_context_module_scm) \
	$(modules_dir)/scheme/process/context/declarations.h \
	$(modules_dir)/scheme/process/context/definitions.c \

auto_scheme_directory_module_scm = $(modules_dir)/auto/scheme/directory/module.scm 
auto_scheme_directory_dep = \
	$(auto_scheme_directory_module_scm) \
	$(modules_dir)/auto/scheme/directory/declarations.h \
	$(modules_dir)/auto/scheme/directory/definitions.c \

auto_scheme_path_module_scm = $(modules_dir)/auto/scheme/path/module.scm 
auto_scheme_path_dep = \
	$(auto_scheme_path_module_scm) \
	$(modules_dir)/auto/scheme/path/declarations.h \
	$(modules_dir)/auto/scheme/path/definitions.c \

scheme_read_module_scm = $(modules_dir)/scheme/read/module.scm 
scheme_read_dep = \
	$(scheme_read_module_scm) \

auto_scheme_file_module_scm = $(modules_dir)/auto/scheme/file/module.scm 
auto_scheme_file_dep = \
	$(auto_scheme_file_module_scm) \

auto_scheme_port_module_scm = $(modules_dir)/auto/scheme/port/module.scm 
auto_scheme_port_dep = \
	$(auto_scheme_port_module_scm) \

auto_scheme_char_module_scm = $(modules_dir)/auto/scheme/char/module.scm 
auto_scheme_char_dep = \
	$(auto_scheme_char_module_scm) \

auto_scheme_inexact_module_scm = $(modules_dir)/auto/scheme/inexact/module.scm 
auto_scheme_inexact_dep = \
	$(auto_scheme_inexact_module_scm) \

auto_scheme_lazy_module_scm = $(modules_dir)/auto/scheme/lazy/module.scm 
auto_scheme_lazy_dep = \
	$(auto_scheme_lazy_module_scm) \

auto_scheme_memory_module_scm = $(modules_dir)/auto/scheme/memory/module.scm 
auto_scheme_memory_dep = \
	$(auto_scheme_memory_module_scm) \

auto_scheme_closure_module_scm = $(modules_dir)/auto/scheme/closure/module.scm 
auto_scheme_closure_dep = \
	$(auto_scheme_closure_module_scm) \

auto_scheme_compile_module_scm = $(modules_dir)/auto/scheme/compile/module.scm 
auto_scheme_compile_dep = \
	$(auto_scheme_compile_module_scm) \

auto_scheme_interpret_module_scm = $(modules_dir)/auto/scheme/interpret/module.scm 
auto_scheme_interpret_dep = \
	$(auto_scheme_interpret_module_scm) \


libautoscheme_obj = \
	$(obj_dir)/bignum.o \
	$(obj_dir)/autoscheme.o \
	$(obj_dir)/scheme.o \
	$(obj_dir)/scheme_read.o \
	$(obj_dir)/scheme_cxr.o \
	$(obj_dir)/auto_scheme_environment.o \
	$(obj_dir)/auto_scheme_macro.o \
	$(obj_dir)/auto_scheme_base.o \
	$(obj_dir)/scheme_eval.o \
	$(obj_dir)/scheme_repl.o \
	$(obj_dir)/scheme_load.o \
	$(obj_dir)/auto_scheme_list.o \
	$(obj_dir)/auto_scheme_write.o \
	$(obj_dir)/auto_scheme_args_fold.o \
	$(obj_dir)/auto_scheme_args.o \
	$(obj_dir)/auto_scheme_string.o \
	$(obj_dir)/scheme_process_context.o \
	$(obj_dir)/auto_scheme_directory.o \
	$(obj_dir)/auto_scheme_path.o \
	$(obj_dir)/auto_scheme_file.o \
	$(obj_dir)/auto_scheme_port.o \
	$(obj_dir)/auto_scheme_char.o \
	$(obj_dir)/auto_scheme_inexact.o \
	$(obj_dir)/auto_scheme_lazy.o \
	$(obj_dir)/auto_scheme_memory.o \
	$(obj_dir)/auto_scheme_closure.o \
	$(obj_dir)/auto_scheme_compile.o \
	$(obj_dir)/auto_scheme_interpret.o \



scheme_modules = scheme \
                 auto_scheme_file \
                 auto_scheme_port \
                 auto_scheme_char \
                 auto_scheme_inexact \
                 auto_scheme_lazy \
                 auto_scheme_memory \
                 auto_scheme_closure \
		 scheme_cxr \
                 auto_scheme_macro \
                 auto_scheme_base \
                 scheme_process_context \
                 scheme_read \
                 auto_scheme_environment \
		 scheme_eval \
		 scheme_repl \
		 scheme_load \
                 auto_scheme_list \
                 auto_scheme_write \
                 auto_scheme_string \
                 auto_scheme_args_fold \
                 auto_scheme_args \
                 auto_scheme_directory \
                 auto_scheme_path \
                 auto_scheme_compile \
                 auto_scheme_interpret \



all: build


build: $(bin_dir)/autoscheme 


$(bin_dir)/autoscheme: $(include_dir)/autoscheme.h $(lib_dir)/libautoscheme.a $(gen_dir)/autoscheme.c
	mkdir -p $(bin_dir)
	$(CC) $(strict_options_89) -o $(bin_dir)/autoscheme $(gen_dir)/autoscheme.c -lm -lautoscheme -L$(lib_dir) -I$(include_dir)

$(gen_dir)/autoscheme.c: $(bin_dir)/autoscheme-prime $(src_dir)/application/autoscheme.scm $(scheme_deps)
	mkdir -p $(gen_dir)
	echo "making $(gen_dir)/scheme.c"
	cd ../../src/application && pwd && ls
	$(bin_dir)/autoscheme-prime --compile $(src_dir)/application/autoscheme.scm -o $(gen_dir)/autoscheme.c --load-modules="$(scheme_modules)"

$(include_dir)/autoscheme.h: $(src_dir)/library/autoscheme.h
	mkdir -p $(include_dir)	
	cp $(src_dir)/library/autoscheme.h $(include_dir)/autoscheme.h

$(lib_dir)/libautoscheme.a: $(libautoscheme_obj)
	mkdir -p $(lib_dir)	
	ar cr $(lib_dir)/libautoscheme.a  $(libautoscheme_obj)



$(obj_dir)/auto_scheme_interpret.o: $(gen_dir)/auto_scheme_interpret.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_interpret.c -o $(obj_dir)/auto_scheme_interpret.o -I$(include_dir)
$(gen_dir)/auto_scheme_interpret.c: $(auto_scheme_interpret_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_interpret_module_scm) -n auto_scheme_interpret -o $(gen_dir)/auto_scheme_interpret.c

$(obj_dir)/auto_scheme_compile.o: $(gen_dir)/auto_scheme_compile.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_compile.c -o $(obj_dir)/auto_scheme_compile.o -I$(include_dir)
$(gen_dir)/auto_scheme_compile.c: $(auto_scheme_compile_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_compile_module_scm) -n auto_scheme_compile -o $(gen_dir)/auto_scheme_compile.c

$(obj_dir)/auto_scheme_file.o: $(gen_dir)/auto_scheme_file.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_file.c -o $(obj_dir)/auto_scheme_file.o -I$(include_dir)
$(gen_dir)/auto_scheme_file.c: $(auto_scheme_file_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_file_module_scm) -n auto_scheme_file -o $(gen_dir)/auto_scheme_file.c

$(obj_dir)/auto_scheme_port.o: $(gen_dir)/auto_scheme_port.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_port.c -o $(obj_dir)/auto_scheme_port.o -I$(include_dir)
$(gen_dir)/auto_scheme_port.c: $(auto_scheme_port_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_port_module_scm) -n auto_scheme_port -o $(gen_dir)/auto_scheme_port.c

$(obj_dir)/auto_scheme_char.o: $(gen_dir)/auto_scheme_char.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_char.c -o $(obj_dir)/auto_scheme_char.o -I$(include_dir)
$(gen_dir)/auto_scheme_char.c: $(auto_scheme_char_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_char_module_scm) -n auto_scheme_char -o $(gen_dir)/auto_scheme_char.c

$(obj_dir)/auto_scheme_inexact.o: $(gen_dir)/auto_scheme_inexact.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_inexact.c -o $(obj_dir)/auto_scheme_inexact.o -I$(include_dir)
$(gen_dir)/auto_scheme_inexact.c: $(auto_scheme_inexact_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_inexact_module_scm) -n auto_scheme_inexact -o $(gen_dir)/auto_scheme_inexact.c

$(obj_dir)/auto_scheme_lazy.o: $(gen_dir)/auto_scheme_lazy.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_lazy.c -o $(obj_dir)/auto_scheme_lazy.o -I$(include_dir)
$(gen_dir)/auto_scheme_lazy.c: $(auto_scheme_lazy_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_lazy_module_scm) -n auto_scheme_lazy -o $(gen_dir)/auto_scheme_lazy.c

$(obj_dir)/auto_scheme_memory.o: $(gen_dir)/auto_scheme_memory.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_memory.c -o $(obj_dir)/auto_scheme_memory.o -I$(include_dir)
$(gen_dir)/auto_scheme_memory.c: $(auto_scheme_memory_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_memory_module_scm) -n auto_scheme_memory -o $(gen_dir)/auto_scheme_memory.c

$(obj_dir)/auto_scheme_closure.o: $(gen_dir)/auto_scheme_closure.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_closure.c -o $(obj_dir)/auto_scheme_closure.o -I$(include_dir)
$(gen_dir)/auto_scheme_closure.c: $(auto_scheme_closure_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_closure_module_scm) -n auto_scheme_closure -o $(gen_dir)/auto_scheme_closure.c

$(obj_dir)/scheme_read.o: $(gen_dir)/scheme_read.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/scheme_read.c -o $(obj_dir)/scheme_read.o -I$(include_dir)
$(gen_dir)/scheme_read.c: $(scheme_read_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(scheme_read_module_scm) -n scheme_read -o $(gen_dir)/scheme_read.c

$(obj_dir)/auto_scheme_path.o: $(gen_dir)/auto_scheme_path.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_path.c -o $(obj_dir)/auto_scheme_path.o -I$(include_dir)
$(gen_dir)/auto_scheme_path.c: $(auto_scheme_path_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_path_module_scm) -n auto_scheme_path -o $(gen_dir)/auto_scheme_path.c

$(obj_dir)/scheme_process_context.o: $(gen_dir)/scheme_process_context.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/scheme_process_context.c -o $(obj_dir)/scheme_process_context.o -I$(include_dir)
$(gen_dir)/scheme_process_context.c: $(scheme_process_context_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(scheme_process_context_module_scm) -n scheme_process_context -o $(gen_dir)/scheme_process_context.c

$(obj_dir)/auto_scheme_directory.o: $(gen_dir)/auto_scheme_directory.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_directory.c -o $(obj_dir)/auto_scheme_directory.o -I$(include_dir)
$(gen_dir)/auto_scheme_directory.c: $(auto_scheme_directory_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_directory_module_scm) -n auto_scheme_directory -o $(gen_dir)/auto_scheme_directory.c


$(obj_dir)/scheme_cxr.o: $(gen_dir)/scheme_cxr.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/scheme_cxr.c -o $(obj_dir)/scheme_cxr.o -I$(include_dir)
$(gen_dir)/scheme_cxr.c: $(scheme_cxr_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(scheme_cxr_module_scm) -n scheme_cxr -o $(gen_dir)/scheme_cxr.c

$(obj_dir)/auto_scheme_string.o: $(gen_dir)/auto_scheme_string.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_string.c -o $(obj_dir)/auto_scheme_string.o -I$(include_dir)
$(gen_dir)/auto_scheme_string.c: $(auto_scheme_string_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_string_module_scm) -n auto_scheme_string -o $(gen_dir)/auto_scheme_string.c

$(obj_dir)/auto_scheme_args.o: $(gen_dir)/auto_scheme_args.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_args.c -o $(obj_dir)/auto_scheme_args.o -I$(include_dir)
$(gen_dir)/auto_scheme_args.c: $(auto_scheme_args_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_args_module_scm) -n auto_scheme_args -o $(gen_dir)/auto_scheme_args.c

$(obj_dir)/auto_scheme_args_fold.o: $(gen_dir)/auto_scheme_args_fold.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_args_fold.c -o $(obj_dir)/auto_scheme_args_fold.o -I$(include_dir)
$(gen_dir)/auto_scheme_args_fold.c: $(auto_scheme_args_fold_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_args_fold_module_scm) -n auto_scheme_args_fold -o $(gen_dir)/auto_scheme_args_fold.c

$(obj_dir)/auto_scheme_write.o: $(gen_dir)/auto_scheme_write.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_write.c -o $(obj_dir)/auto_scheme_write.o -I$(include_dir)
$(gen_dir)/auto_scheme_write.c: $(auto_scheme_write_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_write_module_scm) -n auto_scheme_write -o $(gen_dir)/auto_scheme_write.c

$(obj_dir)/auto_scheme_list.o: $(gen_dir)/auto_scheme_list.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_list.c -o $(obj_dir)/auto_scheme_list.o -I$(include_dir)
$(gen_dir)/auto_scheme_list.c: $(auto_scheme_list_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_list_module_scm) -n auto_scheme_list -o $(gen_dir)/auto_scheme_list.c

$(obj_dir)/scheme_eval.o: $(gen_dir)/scheme_eval.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/scheme_eval.c -o $(obj_dir)/scheme_eval.o -I$(include_dir)
$(gen_dir)/scheme_eval.c: $(scheme_eval_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(scheme_eval_module_scm) -n scheme_eval -o $(gen_dir)/scheme_eval.c

$(obj_dir)/scheme_repl.o: $(gen_dir)/scheme_repl.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/scheme_repl.c -o $(obj_dir)/scheme_repl.o -I$(include_dir)
$(gen_dir)/scheme_repl.c: $(scheme_repl_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(scheme_repl_module_scm) -n scheme_repl -o $(gen_dir)/scheme_repl.c

$(obj_dir)/scheme_load.o: $(gen_dir)/scheme_load.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/scheme_load.c -o $(obj_dir)/scheme_load.o -I$(include_dir)
$(gen_dir)/scheme_load.c: $(scheme_load_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(scheme_load_module_scm) -n scheme_load -o $(gen_dir)/scheme_load.c



$(obj_dir)/auto_scheme_base.o: $(gen_dir)/auto_scheme_base.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_base.c -o $(obj_dir)/auto_scheme_base.o -I$(include_dir)
$(gen_dir)/auto_scheme_base.c: $(auto_scheme_base_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_base_module_scm) -n auto_scheme_base -o $(gen_dir)/auto_scheme_base.c

$(obj_dir)/auto_scheme_macro.o: $(gen_dir)/auto_scheme_macro.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_macro.c -o $(obj_dir)/auto_scheme_macro.o -I$(include_dir)
$(gen_dir)/auto_scheme_macro.c: $(auto_scheme_macro_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_macro_module_scm) -n auto_scheme_macro -o $(gen_dir)/auto_scheme_macro.c

$(obj_dir)/scheme.o: $(gen_dir)/scheme.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/scheme.c -o $(obj_dir)/scheme.o -I$(include_dir)
$(gen_dir)/scheme.c: $(scheme_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(scheme_module_scm) -n scheme -o $(gen_dir)/scheme.c

$(obj_dir)/auto_scheme_environment.o: $(gen_dir)/auto_scheme_environment.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_environment.c -o $(obj_dir)/auto_scheme_environment.o -I$(include_dir)
$(gen_dir)/auto_scheme_environment.c: $(auto_scheme_environment_dep) $(bin_dir)/autoscheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile-module $(auto_scheme_environment_module_scm) -n auto_scheme_environment -o $(gen_dir)/auto_scheme_environment.c





$(obj_dir)/autoscheme.o: $(src_dir)/library/autoscheme.c $(src_dir)/library/autoscheme.h
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(src_dir)/library/autoscheme.c -o $(obj_dir)/autoscheme.o

$(obj_dir)/bignum.o: $(src_dir)/library/bignum.c $(src_dir)/library/bignum.h
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_99) -c $(src_dir)/library/bignum.c -o $(obj_dir)/bignum.o






#####################


$(bin_dir)/autoscheme-prime:
	mkdir -p $(bin_dir)
	$(CC) $(strict_options_99) -o $(bin_dir)/autoscheme-prime -I$(src_dir)/library -lm $(src_dir)/library/bignum.c $(src_dir)/library/autoscheme.c \
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



install: $(bin_dir)/scheme
	cp $(bin_dir)/scheme $(install_dir)/

uninstall: 
	rm $(install_dir)/scheme

clean:
	rm -rf $(gen_dir)
	rm -rf $(obj_dir)
	rm -rf $(bin_dir)
	rm -rf $(include_dir)
	rm -rf $(lib_dir)

	rm -rf tmp
