#  This file is part of the 'AutoScheme' project.
#  Copyright 2021 Steven Wiley <s.wiley@katchitek.com> 
#  SPDX-License-Identifier: BSD-2-Clause

project_dir = ../..
src_dir = $(project_dir)/src
prime_dir = $(src_dir)/prime
s7_dir = $(src_dir)/s7
_modules_dir = $(src_dir)/_modules
modules_dir = $(src_dir)/modules
gen_dir = $(project_dir)/gen
bin_dir = bin
obj_dir = _obj
lib_dir = lib
include_dir = include

install_dir = /usr/local/bin

strict_options = -Wall -Wextra -pedantic -Wmissing-prototypes -Wstrict-prototypes -Wold-style-definition -std=c89 
compile_options = -fbracket-depth=10000 $(strict_options) 


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


scheme_deps = \
	$(modules_dir)/auto/scheme/environment/module.scm \
	$(modules_dir)/auto/scheme/environment/procedures.scm \
	$(modules_dir)/auto/scheme/environment/declarations.h \
	$(modules_dir)/auto/scheme/environment/definitions.c \
	$(modules_dir)/auto/scheme/environment/initialization.c \
	$(modules_dir)/auto/scheme/environment/macros.scm 



all: build

build: $(bin_dir)/scheme 


$(bin_dir)/scheme: $(include_dir)/autoscheme.h $(lib_dir)/libautoscheme.a $(gen_dir)/scheme.c
	$(CC) -o $(bin_dir)/scheme $(gen_dir)/scheme.c -lautoscheme -L$(lib_dir) -I$(include_dir)


$(gen_dir)/scheme.c: $(bin_dir)/autoscheme $(src_dir)/application/scheme.scm $(scheme_deps)
	mkdir -p $(gen_dir)
	echo "making $(gen_dir)/scheme.c"
	cd ../../src/application && pwd && ls
	$(bin_dir)/autoscheme --compile $(src_dir)/application/scheme.scm -o $(gen_dir)/scheme.c 







$(include_dir)/autoscheme.h: $(src_dir)/library/autoscheme.h
	mkdir -p $(include_dir)	
	cp $(src_dir)/library/autoscheme.h $(include_dir)/autoscheme.h

$(lib_dir)/libautoscheme.a: $(obj_dir)/bignum.o $(obj_dir)/autoscheme.o 
	mkdir -p $(lib_dir)	
	ar cr $(lib_dir)/libautoscheme.a $(obj_dir)/bignum.o $(obj_dir)/autoscheme.o 



$(obj_dir)/autoscheme.o: $(src_dir)/library/autoscheme.c $(src_dir)/library/autoscheme.h
	mkdir -p $(obj_dir)
	$(CC) -c $(src_dir)/library/autoscheme.c -o $(obj_dir)/autoscheme.o

$(obj_dir)/bignum.o: $(src_dir)/library/bignum.c $(src_dir)/library/bignum.h
	mkdir -p $(obj_dir)
	$(CC) -c $(src_dir)/library/bignum.c -o $(obj_dir)/bignum.o




$(bin_dir)/autoscheme: $(obj_dir)/s7.o $(gen_dir)/auto_scheme_environment_module.c $(gen_dir)/s7_module.c $(gen_dir)/program.c 
	mkdir -p $(bin_dir)
	$(CC) $(compile_options) -I$(src_dir) -o $(bin_dir)/autoscheme $(obj_dir)/s7.o $(gen_dir)/auto_scheme_environment_module.c $(gen_dir)/s7_module.c $(gen_dir)/program.c


$(gen_dir)/program.c: $(bin_dir)/autoscheme-prime $(autoscheme_sources) 
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile $(autoscheme_sources) -o $(gen_dir)/program.c --link-modules="auto_scheme_environment s7"



$(gen_dir)/s7_module.c: $(bin_dir)/autoscheme-prime $(_modules_dir)/s7/module.scm
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime -m $(_modules_dir)/s7/module.scm -n s7 -o $(gen_dir)/s7_module.c



$(gen_dir)/auto_scheme_environment_module.c: $(bin_dir)/autoscheme-prime $(_modules_dir)/auto/scheme/environment/module.scm 
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime -m $(_modules_dir)/auto/scheme/environment/module.scm -n auto_scheme_environment -o $(gen_dir)/auto_scheme_environment_module.c








$(bin_dir)/autoscheme-prime: $(obj_dir)/s7.o $(prime_dir)/auto_scheme_environment_module.c $(prime_dir)/s7_module.c $(prime_dir)/program.c
	mkdir -p $(bin_dir)
	$(CC) $(compile_options) -I$(src_dir) -o $(bin_dir)/autoscheme-prime $(obj_dir)/s7.o $(prime_dir)/auto_scheme_environment_module.c $(prime_dir)/s7_module.c $(prime_dir)/program.c 




$(obj_dir)/s7.o: $(s7_dir)/s7.h $(s7_dir)/s7.c
	mkdir -p $(obj_dir)
	$(CC) -o $(obj_dir)/s7.o -c $(s7_dir)/s7.c




install: $(bin_dir)/autoscheme
	cp $(bin_dir)/autoscheme $(install_dir)/

uninstall: 
	rm $(install_dir)/autoscheme

clean:
	rm -rf $(gen_dir)
	rm -rf $(obj_dir)
	rm -rf $(bin_dir)
	rm -rf $(include_dir)
	rm -rf $(lib_dir)

	rm -rf tmp
