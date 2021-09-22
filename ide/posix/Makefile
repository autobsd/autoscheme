#  This file is part of the 'AutoScheme' project.
#  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
#  SPDX-License-Identifier: BSD-2-Clause

project_dir = ../..
src_dir = $(project_dir)/src
prime_dir = $(src_dir)/prime
s7_dir = $(src_dir)/s7
_modules_dir = $(src_dir)/_modules
modules_dir = $(src_dir)/modules
_gen_dir = $(project_dir)/_gen
gen_dir = $(project_dir)/gen
bin_dir = bin
_bin_dir = _bin
obj_dir = obj
_obj_dir = _obj
lib_dir = lib
include_dir = include

install_dir = /usr/local/bin

strict_options = -Wall -Wextra -pedantic -Wmissing-prototypes -Wstrict-prototypes -Wold-style-definition -std=c89 
compile_options = -fbracket-depth=10000 # $(strict_options) 


library_sources = \
	$(_modules_dir)/auto/scheme/thread/module.scm \
	$(_modules_dir)/auto/scheme/base/module.scm \
	$(_modules_dir)/auto/scheme/eval/module.scm \
	$(_modules_dir)/auto/scheme/load/module.scm \
	$(_modules_dir)/auto/scheme/read/module.scm \
	$(_modules_dir)/auto/scheme/repl/module.scm \
	$(_modules_dir)/auto/scheme/cxr/module.scm \
	$(_modules_dir)/auto/scheme/path/module.scm \
	$(_modules_dir)/auto/scheme/write/module.scm \
	$(_modules_dir)/auto/scheme/string/module.scm \
	$(_modules_dir)/auto/scheme/file/module.scm \
	$(_modules_dir)/auto/scheme/args/fold/module.scm \
	$(_modules_dir)/auto/scheme/process/context/module.scm \
	$(_modules_dir)/auto/scheme/compile/module.scm \
	$(_modules_dir)/auto/scheme/interpret/module.scm \
	$(_modules_dir)/auto/scheme/repl/module.scm \
	$(_modules_dir)/auto/scheme/args/module.scm

autoscheme_sources =  \
	$(library_sources) \
	$(src_dir)/application/program.scm 



auto_scheme_module_scm = $(modules_dir)/auto/scheme/module.scm 
auto_scheme_dep = \
	$(auto_scheme_module_scm) \
	$(modules_dir)/auto/scheme/declarations.h \
	$(modules_dir)/auto/scheme/definitions.c \
	$(modules_dir)/auto/scheme/initialization.c \
	$(modules_dir)/auto/scheme/identifiers.scm \
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
	$(modules_dir)/auto/scheme/environment/declarations.h \
	$(modules_dir)/auto/scheme/environment/definitions.c \
	$(modules_dir)/auto/scheme/environment/initialization.c \
	$(modules_dir)/auto/scheme/environment/library.scm \



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


libautoscheme_obj = \
	$(obj_dir)/bignum.o \
	$(obj_dir)/autoscheme.o \
	$(obj_dir)/auto_scheme.o \
	$(obj_dir)/auto_scheme_environment.o \
	$(obj_dir)/auto_scheme_library.o \
	$(obj_dir)/auto_scheme_base.o \
	$(obj_dir)/auto_scheme_eval.o \
	$(obj_dir)/auto_scheme_list.o \



scheme_modules = auto_scheme auto_scheme_environment auto_scheme_environment
# auto_scheme_base auto_scheme_eval auto_scheme_list


all: build

build: $(bin_dir)/scheme 


$(bin_dir)/scheme: $(include_dir)/autoscheme.h $(lib_dir)/libautoscheme.a $(gen_dir)/scheme.c
	mkdir -p $(bin_dir)
	$(CC) $(compile_options) -o $(bin_dir)/scheme $(gen_dir)/scheme.c -lautoscheme -L$(lib_dir) -I$(include_dir)

$(gen_dir)/scheme.c: $(_bin_dir)/autoscheme $(src_dir)/application/scheme.scm $(scheme_deps)
	mkdir -p $(gen_dir)
	echo "making $(gen_dir)/scheme.c"
	cd ../../src/application && pwd && ls
	$(_bin_dir)/autoscheme --compile $(src_dir)/application/scheme.scm -o $(gen_dir)/scheme.c --load-modules="$(scheme_modules)"




$(include_dir)/autoscheme.h: $(src_dir)/library/autoscheme.h
	mkdir -p $(include_dir)	
	cp $(src_dir)/library/autoscheme.h $(include_dir)/autoscheme.h




$(lib_dir)/libautoscheme.a: $(libautoscheme_obj)
	mkdir -p $(lib_dir)	
	ar cr $(lib_dir)/libautoscheme.a  $(libautoscheme_obj)





$(obj_dir)/auto_scheme_list.o: $(gen_dir)/auto_scheme_list.c
	mkdir -p $(obj_dir)
	$(CC) $(compile_options) -c $(gen_dir)/auto_scheme_list.c -o $(obj_dir)/auto_scheme_list.o -I$(include_dir)

$(gen_dir)/auto_scheme_list.c: $(auto_scheme_list_dep) $(_bin_dir)/autoscheme
	mkdir -p $(gen_dir)
	$(_bin_dir)/autoscheme --compile-module $(auto_scheme_list_module_scm) -n auto_scheme_list -o $(gen_dir)/auto_scheme_list.c


$(obj_dir)/auto_scheme_eval.o: $(gen_dir)/auto_scheme_eval.c
	mkdir -p $(obj_dir)
	$(CC) $(compile_options) -c $(gen_dir)/auto_scheme_eval.c -o $(obj_dir)/auto_scheme_eval.o -I$(include_dir)

$(gen_dir)/auto_scheme_eval.c: $(auto_scheme_eval_dep) $(_bin_dir)/autoscheme
	mkdir -p $(gen_dir)
	$(_bin_dir)/autoscheme --compile-module $(auto_scheme_eval_module_scm) -n auto_scheme_eval -o $(gen_dir)/auto_scheme_eval.c



$(obj_dir)/auto_scheme_base.o: $(gen_dir)/auto_scheme_base.c
	mkdir -p $(obj_dir)
	$(CC) $(compile_options) -c $(gen_dir)/auto_scheme_base.c -o $(obj_dir)/auto_scheme_base.o -I$(include_dir)

$(gen_dir)/auto_scheme_base.c: $(auto_scheme_base_dep) $(_bin_dir)/autoscheme
	mkdir -p $(gen_dir)
	$(_bin_dir)/autoscheme --compile-module $(auto_scheme_base_module_scm) -n auto_scheme_base -o $(gen_dir)/auto_scheme_base.c



$(obj_dir)/auto_scheme.o: $(gen_dir)/auto_scheme.c
	mkdir -p $(obj_dir)
	$(CC) $(compile_options) -c $(gen_dir)/auto_scheme.c -o $(obj_dir)/auto_scheme.o -I$(include_dir)

$(gen_dir)/auto_scheme.c: $(auto_scheme_dep) $(_bin_dir)/autoscheme
	mkdir -p $(gen_dir)
	$(_bin_dir)/autoscheme --compile-module $(auto_scheme_module_scm) -n auto_scheme -o $(gen_dir)/auto_scheme.c



$(obj_dir)/auto_scheme_library.o: $(gen_dir)/auto_scheme_library.c
	mkdir -p $(obj_dir)
	$(CC) $(compile_options) -c $(gen_dir)/auto_scheme_library.c -o $(obj_dir)/auto_scheme_library.o -I$(include_dir)

$(gen_dir)/auto_scheme_library.c: $(auto_scheme_library_dep) $(_bin_dir)/autoscheme
	mkdir -p $(gen_dir)
	$(_bin_dir)/autoscheme --compile-module $(auto_scheme_library_module_scm) -n auto_scheme_library -o $(gen_dir)/auto_scheme_library.c



$(obj_dir)/auto_scheme_environment.o: $(gen_dir)/auto_scheme_environment.c
	mkdir -p $(obj_dir)
	$(CC) $(compile_options) -c $(gen_dir)/auto_scheme_environment.c -o $(obj_dir)/auto_scheme_environment.o -I$(include_dir)

$(gen_dir)/auto_scheme_environment.c: $(auto_scheme_environment_dep) $(_bin_dir)/autoscheme
	mkdir -p $(gen_dir)
	$(_bin_dir)/autoscheme --compile-module $(auto_scheme_environment_module_scm) -n auto_scheme_environment -o $(gen_dir)/auto_scheme_environment.c



# $(obj_dir)/_auto_scheme.o: $(gen_dir)/_auto_scheme.c
# 	mkdir -p $(obj_dir)
# 	$(CC) $(compile_options) -c $(gen_dir)/_auto_scheme.c -o $(obj_dir)/_auto_scheme.o -I$(include_dir)

# $(gen_dir)/_auto_scheme.c: $(_auto_scheme_dep) $(_bin_dir)/autoscheme
# 	mkdir -p $(gen_dir)
# 	$(_bin_dir)/autoscheme --compile-module $(_auto_scheme_module_scm) -n _auto_scheme -o $(gen_dir)/_auto_scheme.c






$(obj_dir)/autoscheme.o: $(src_dir)/library/autoscheme.c $(src_dir)/library/autoscheme.h
	mkdir -p $(obj_dir)
	$(CC) $(compile_options) -c $(src_dir)/library/autoscheme.c -o $(obj_dir)/autoscheme.o

$(obj_dir)/bignum.o: $(src_dir)/library/bignum.c $(src_dir)/library/bignum.h
	mkdir -p $(obj_dir)
	$(CC) $(compile_options) -c $(src_dir)/library/bignum.c -o $(obj_dir)/bignum.o






#####################



$(_bin_dir)/autoscheme: $(_obj_dir)/s7.o $(_gen_dir)/auto_scheme_environment_module.c $(_gen_dir)/s7_module.c $(_gen_dir)/program.c 
	mkdir -p $(_bin_dir)
	$(CC) $(compile_options) -I$(src_dir) -o $(_bin_dir)/autoscheme $(_obj_dir)/s7.o $(_gen_dir)/auto_scheme_environment_module.c $(_gen_dir)/s7_module.c $(_gen_dir)/program.c


$(_gen_dir)/program.c: $(_bin_dir)/autoscheme-prime $(autoscheme_sources) 
	mkdir -p $(_gen_dir)
	$(_bin_dir)/autoscheme-prime --compile $(autoscheme_sources) -o $(_gen_dir)/program.c --link-modules="auto_scheme_environment s7"



$(_gen_dir)/s7_module.c: $(_bin_dir)/autoscheme-prime $(_modules_dir)/s7/module.scm
	mkdir -p $(_gen_dir)
	$(_bin_dir)/autoscheme-prime -m $(_modules_dir)/s7/module.scm -n s7 -o $(_gen_dir)/s7_module.c



$(_gen_dir)/auto_scheme_environment_module.c: $(_bin_dir)/autoscheme-prime $(_modules_dir)/auto/scheme/environment/module.scm 
	mkdir -p $(_gen_dir)
	$(_bin_dir)/autoscheme-prime -m $(_modules_dir)/auto/scheme/environment/module.scm -n auto_scheme_environment -o $(_gen_dir)/auto_scheme_environment_module.c








$(_bin_dir)/autoscheme-prime: $(_obj_dir)/s7.o $(prime_dir)/auto_scheme_environment_module.c $(prime_dir)/s7_module.c $(prime_dir)/program.c
	mkdir -p $(_bin_dir)
	$(CC) $(compile_options) -I$(src_dir) -o $(_bin_dir)/autoscheme-prime $(_obj_dir)/s7.o $(prime_dir)/auto_scheme_environment_module.c $(prime_dir)/s7_module.c $(prime_dir)/program.c 




$(_obj_dir)/s7.o: $(s7_dir)/s7.h $(s7_dir)/s7.c
	mkdir -p $(_obj_dir)
	$(CC) -o $(_obj_dir)/s7.o -c $(s7_dir)/s7.c




install: $(_bin_dir)/autoscheme
	cp $(_bin_dir)/autoscheme $(install_dir)/

uninstall: 
	rm $(install_dir)/autoscheme

clean:
	rm -rf $(gen_dir)
	rm -rf $(_gen_dir)
	rm -rf $(obj_dir)
	rm -rf $(_obj_dir)
	rm -rf $(bin_dir)
	rm -rf $(_bin_dir)
	rm -rf $(include_dir)
	rm -rf $(lib_dir)

	rm -rf tmp
