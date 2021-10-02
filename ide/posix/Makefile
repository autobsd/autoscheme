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



auto_scheme_module_scm = $(modules_dir)/auto/scheme/module.scm 
auto_scheme_dep = \
	$(auto_scheme_module_scm) \
	$(modules_dir)/auto/scheme/identifiers.scm \
	$(modules_dir)/auto/scheme/procedures.c \
	$(modules_dir)/auto/scheme/environment/declarations.h \
	$(modules_dir)/auto/scheme/environment/definitions.c \
	$(modules_dir)/auto/scheme/environment/initialization.c \
	$(modules_dir)/auto/scheme/library/procedures.scm \
	$(modules_dir)/auto/scheme/library/macros.scm \
	$(modules_dir)/auto/scheme/library/module.scm \
	$(modules_dir)/auto/scheme/library/library.scm \


auto_scheme_environment_module_scm = $(modules_dir)/auto/scheme/environment/module.scm 
auto_scheme_environment_dep = \
	$(auto_scheme_environment_module_scm) \



auto_scheme_base_module_scm = $(modules_dir)/auto/scheme/base/module.scm 
auto_scheme_base_dep = \
	$(auto_scheme_base_module_scm) \



auto_scheme_library_module_scm = $(modules_dir)/auto/scheme/library/module.scm 
auto_scheme_library_dep = \
	$(auto_scheme_library_module_scm) \
	$(modules_dir)/auto/scheme/library/procedures.scm \
	$(modules_dir)/auto/scheme/library/macros.scm \
	$(modules_dir)/auto/scheme/library/library.scm \




auto_scheme_eval_module_scm = $(modules_dir)/auto/scheme/eval/module.scm 
auto_scheme_eval_dep = \
	$(auto_scheme_eval_module_scm) \



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

auto_scheme_cxr_module_scm = $(modules_dir)/auto/scheme/cxr/module.scm 
auto_scheme_cxr_dep = \
	$(auto_scheme_cxr_module_scm) \

auto_scheme_process_context_module_scm = $(modules_dir)/auto/scheme/process/context/module.scm 
auto_scheme_process_context_dep = \
	$(auto_scheme_process_context_module_scm) \
	$(modules_dir)/auto/scheme/process/context/declarations.h \
	$(modules_dir)/auto/scheme/process/context/definitions.c \
	$(modules_dir)/auto/scheme/process/context/initialization.c \

auto_scheme_path_module_scm = $(modules_dir)/auto/scheme/path/module.scm 
auto_scheme_path_dep = \
	$(auto_scheme_path_module_scm) \

auto_scheme_read_module_scm = $(modules_dir)/auto/scheme/read/module.scm 
auto_scheme_read_dep = \
	$(auto_scheme_read_module_scm) \

auto_scheme_file_module_scm = $(modules_dir)/auto/scheme/file/module.scm 
auto_scheme_file_dep = \
	$(auto_scheme_file_module_scm) \

auto_scheme_compile_module_scm = $(modules_dir)/auto/scheme/compile/module.scm 
auto_scheme_compile_dep = \
	$(auto_scheme_compile_module_scm) \

auto_scheme_interpret_module_scm = $(modules_dir)/auto/scheme/interpret/module.scm 
auto_scheme_interpret_dep = \
	$(auto_scheme_interpret_module_scm) \
	$(modules_dir)/auto/scheme/interpret/declarations.h \
	$(modules_dir)/auto/scheme/interpret/definitions.c \


libautoscheme_obj = \
	$(obj_dir)/bignum.o \
	$(obj_dir)/autoscheme.o \
	$(obj_dir)/auto_scheme.o \
	$(obj_dir)/auto_scheme_environment.o \
	$(obj_dir)/auto_scheme_library.o \
	$(obj_dir)/auto_scheme_base.o \
	$(obj_dir)/auto_scheme_eval.o \
	$(obj_dir)/auto_scheme_list.o \
	$(obj_dir)/auto_scheme_write.o \
	$(obj_dir)/auto_scheme_args_fold.o \
	$(obj_dir)/auto_scheme_args.o \
	$(obj_dir)/auto_scheme_string.o \
	$(obj_dir)/auto_scheme_cxr.o \
	$(obj_dir)/auto_scheme_process_context.o \
	$(obj_dir)/auto_scheme_path.o \
	$(obj_dir)/auto_scheme_read.o \
	$(obj_dir)/auto_scheme_file.o \
	$(obj_dir)/auto_scheme_compile.o \
	$(obj_dir)/auto_scheme_interpret.o \



scheme_modules = auto_scheme \
                 auto_scheme_base \
                 auto_scheme_environment \
                 auto_scheme_eval \
                 auto_scheme_list \
                 auto_scheme_write \
                 auto_scheme_cxr \
                 auto_scheme_string \
                 auto_scheme_args_fold \
                 auto_scheme_args \
                 auto_scheme_process_context \
                 auto_scheme_path \
                 auto_scheme_read \
                 auto_scheme_file \
                 auto_scheme_compile \
                 auto_scheme_interpret \
              #  auto_scheme_library



all: build


build: $(bin_dir)/scheme 


$(bin_dir)/scheme: $(include_dir)/autoscheme.h $(lib_dir)/libautoscheme.a $(gen_dir)/scheme.c
	mkdir -p $(bin_dir)
	$(CC) $(strict_options_89) -o $(bin_dir)/scheme $(gen_dir)/scheme.c -lautoscheme -L$(lib_dir) -I$(include_dir)

$(gen_dir)/scheme.c: $(bin_dir)/scheme-prime $(src_dir)/application/scheme.scm $(scheme_deps)
	mkdir -p $(gen_dir)
	echo "making $(gen_dir)/scheme.c"
	cd ../../src/application && pwd && ls
	$(bin_dir)/scheme-prime --compile $(src_dir)/application/scheme.scm -o $(gen_dir)/scheme.c --load-modules="$(scheme_modules)"

$(include_dir)/autoscheme.h: $(src_dir)/library/autoscheme.h
	mkdir -p $(include_dir)	
	cp $(src_dir)/library/autoscheme.h $(include_dir)/autoscheme.h

$(lib_dir)/libautoscheme.a: $(libautoscheme_obj)
	mkdir -p $(lib_dir)	
	ar cr $(lib_dir)/libautoscheme.a  $(libautoscheme_obj)



$(obj_dir)/auto_scheme_interpret.o: $(gen_dir)/auto_scheme_interpret.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_interpret.c -o $(obj_dir)/auto_scheme_interpret.o -I$(include_dir)
$(gen_dir)/auto_scheme_interpret.c: $(auto_scheme_interpret_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_interpret_module_scm) -n auto_scheme_interpret -o $(gen_dir)/auto_scheme_interpret.c

$(obj_dir)/auto_scheme_compile.o: $(gen_dir)/auto_scheme_compile.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_compile.c -o $(obj_dir)/auto_scheme_compile.o -I$(include_dir)
$(gen_dir)/auto_scheme_compile.c: $(auto_scheme_compile_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_compile_module_scm) -n auto_scheme_compile -o $(gen_dir)/auto_scheme_compile.c

$(obj_dir)/auto_scheme_file.o: $(gen_dir)/auto_scheme_file.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_file.c -o $(obj_dir)/auto_scheme_file.o -I$(include_dir)
$(gen_dir)/auto_scheme_file.c: $(auto_scheme_file_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_file_module_scm) -n auto_scheme_file -o $(gen_dir)/auto_scheme_file.c

$(obj_dir)/auto_scheme_read.o: $(gen_dir)/auto_scheme_read.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_read.c -o $(obj_dir)/auto_scheme_read.o -I$(include_dir)
$(gen_dir)/auto_scheme_read.c: $(auto_scheme_read_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_read_module_scm) -n auto_scheme_read -o $(gen_dir)/auto_scheme_read.c

$(obj_dir)/auto_scheme_path.o: $(gen_dir)/auto_scheme_path.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_path.c -o $(obj_dir)/auto_scheme_path.o -I$(include_dir)
$(gen_dir)/auto_scheme_path.c: $(auto_scheme_path_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_path_module_scm) -n auto_scheme_path -o $(gen_dir)/auto_scheme_path.c

$(obj_dir)/auto_scheme_process_context.o: $(gen_dir)/auto_scheme_process_context.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_process_context.c -o $(obj_dir)/auto_scheme_process_context.o -I$(include_dir)
$(gen_dir)/auto_scheme_process_context.c: $(auto_scheme_process_context_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_process_context_module_scm) -n auto_scheme_process_context -o $(gen_dir)/auto_scheme_process_context.c

$(obj_dir)/auto_scheme_cxr.o: $(gen_dir)/auto_scheme_cxr.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_cxr.c -o $(obj_dir)/auto_scheme_cxr.o -I$(include_dir)
$(gen_dir)/auto_scheme_cxr.c: $(auto_scheme_cxr_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_cxr_module_scm) -n auto_scheme_cxr -o $(gen_dir)/auto_scheme_cxr.c

$(obj_dir)/auto_scheme_string.o: $(gen_dir)/auto_scheme_string.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_string.c -o $(obj_dir)/auto_scheme_string.o -I$(include_dir)
$(gen_dir)/auto_scheme_string.c: $(auto_scheme_string_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_string_module_scm) -n auto_scheme_string -o $(gen_dir)/auto_scheme_string.c

$(obj_dir)/auto_scheme_args.o: $(gen_dir)/auto_scheme_args.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_args.c -o $(obj_dir)/auto_scheme_args.o -I$(include_dir)
$(gen_dir)/auto_scheme_args.c: $(auto_scheme_args_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_args_module_scm) -n auto_scheme_args -o $(gen_dir)/auto_scheme_args.c

$(obj_dir)/auto_scheme_args_fold.o: $(gen_dir)/auto_scheme_args_fold.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_args_fold.c -o $(obj_dir)/auto_scheme_args_fold.o -I$(include_dir)
$(gen_dir)/auto_scheme_args_fold.c: $(auto_scheme_args_fold_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_args_fold_module_scm) -n auto_scheme_args_fold -o $(gen_dir)/auto_scheme_args_fold.c

$(obj_dir)/auto_scheme_write.o: $(gen_dir)/auto_scheme_write.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_write.c -o $(obj_dir)/auto_scheme_write.o -I$(include_dir)
$(gen_dir)/auto_scheme_write.c: $(auto_scheme_write_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_write_module_scm) -n auto_scheme_write -o $(gen_dir)/auto_scheme_write.c

$(obj_dir)/auto_scheme_list.o: $(gen_dir)/auto_scheme_list.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_list.c -o $(obj_dir)/auto_scheme_list.o -I$(include_dir)
$(gen_dir)/auto_scheme_list.c: $(auto_scheme_list_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_list_module_scm) -n auto_scheme_list -o $(gen_dir)/auto_scheme_list.c

$(obj_dir)/auto_scheme_eval.o: $(gen_dir)/auto_scheme_eval.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_eval.c -o $(obj_dir)/auto_scheme_eval.o -I$(include_dir)
$(gen_dir)/auto_scheme_eval.c: $(auto_scheme_eval_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_eval_module_scm) -n auto_scheme_eval -o $(gen_dir)/auto_scheme_eval.c

$(obj_dir)/auto_scheme_base.o: $(gen_dir)/auto_scheme_base.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_base.c -o $(obj_dir)/auto_scheme_base.o -I$(include_dir)
$(gen_dir)/auto_scheme_base.c: $(auto_scheme_base_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_base_module_scm) -n auto_scheme_base -o $(gen_dir)/auto_scheme_base.c

$(obj_dir)/auto_scheme.o: $(gen_dir)/auto_scheme.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme.c -o $(obj_dir)/auto_scheme.o -I$(include_dir)
$(gen_dir)/auto_scheme.c: $(auto_scheme_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_module_scm) -n auto_scheme -o $(gen_dir)/auto_scheme.c

$(obj_dir)/auto_scheme_library.o: $(gen_dir)/auto_scheme_library.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_library.c -o $(obj_dir)/auto_scheme_library.o -I$(include_dir)
$(gen_dir)/auto_scheme_library.c: $(auto_scheme_library_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_library_module_scm) -n auto_scheme_library -o $(gen_dir)/auto_scheme_library.c

$(obj_dir)/auto_scheme_environment.o: $(gen_dir)/auto_scheme_environment.c
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(gen_dir)/auto_scheme_environment.c -o $(obj_dir)/auto_scheme_environment.o -I$(include_dir)
$(gen_dir)/auto_scheme_environment.c: $(auto_scheme_environment_dep) $(bin_dir)/scheme-prime
	mkdir -p $(gen_dir)
	$(bin_dir)/scheme-prime --compile-module $(auto_scheme_environment_module_scm) -n auto_scheme_environment -o $(gen_dir)/auto_scheme_environment.c





$(obj_dir)/autoscheme.o: $(src_dir)/library/autoscheme.c $(src_dir)/library/autoscheme.h
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_89) -c $(src_dir)/library/autoscheme.c -o $(obj_dir)/autoscheme.o

$(obj_dir)/bignum.o: $(src_dir)/library/bignum.c $(src_dir)/library/bignum.h
	mkdir -p $(obj_dir)
	$(CC) $(strict_options_99) -c $(src_dir)/library/bignum.c -o $(obj_dir)/bignum.o






#####################


$(bin_dir)/scheme-prime:
	mkdir -p $(bin_dir)
	$(CC) $(strict_options_99) -o $(bin_dir)/scheme-prime -I$(src_dir)/library $(src_dir)/library/bignum.c $(src_dir)/library/autoscheme.c \
		$(prime_dir)/auto_scheme.c \
		$(prime_dir)/auto_scheme_base.c \
		$(prime_dir)/auto_scheme_write.c \
		$(prime_dir)/auto_scheme_string.c \
		$(prime_dir)/auto_scheme_read.c \
		$(prime_dir)/auto_scheme_process_context.c \
		$(prime_dir)/auto_scheme_path.c \
		$(prime_dir)/auto_scheme_list.c \
		$(prime_dir)/auto_scheme_file.c \
		$(prime_dir)/auto_scheme_eval.c \
		$(prime_dir)/auto_scheme_environment.c \
		$(prime_dir)/auto_scheme_cxr.c \
		$(prime_dir)/auto_scheme_compile.c \
		$(prime_dir)/auto_scheme_interpret.c \
		$(prime_dir)/auto_scheme_args.c \
		$(prime_dir)/auto_scheme_args_fold.c \
		$(prime_dir)/scheme.c  


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
