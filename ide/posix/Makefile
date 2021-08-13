project_dir = ../..
src_dir = $(project_dir)/src
prime_dir = $(src_dir)/prime
s7_dir = $(src_dir)/s7
mod_dir = $(src_dir)/modules
gen_dir = $(project_dir)/gen
bin_dir = bin
obj_dir = _obj

install_dir = /usr/local/bin

strict_options = -Wall -Wextra -pedantic -Wmissing-prototypes -Wstrict-prototypes -Wold-style-definition -std=c89 

library_sources = \
	$(mod_dir)/auto/scheme/base/module.scm \
	$(mod_dir)/auto/scheme/eval/module.scm \
	$(mod_dir)/auto/scheme/load/module.scm \
	$(mod_dir)/auto/scheme/read/module.scm \
	$(mod_dir)/auto/scheme/repl/module.scm \
	$(mod_dir)/auto/scheme/cxr/module.scm \
	$(mod_dir)/auto/scheme/path/module.scm \
	$(mod_dir)/auto/scheme/write/module.scm \
	$(mod_dir)/auto/scheme/string/module.scm \
	$(mod_dir)/auto/scheme/args/fold/module.scm

autoscheme_sources =  \
	$(library_sources) \
	$(src_dir)/usage.scm \
	$(src_dir)/compiler.scm \
	$(src_dir)/interpreter.scm \
	$(src_dir)/repl.scm \
	$(src_dir)/program.scm 

all: build

build: $(bin_dir)/autoscheme



$(bin_dir)/autoscheme: $(obj_dir)/s7.o $(gen_dir)/auto_scheme_environment_module.c $(gen_dir)/s7_module.c $(gen_dir)/program.c 
	mkdir -p $(bin_dir)
	cc $(strict_options) -I$(src_dir) -o $(bin_dir)/autoscheme $(obj_dir)/s7.o $(gen_dir)/auto_scheme_environment_module.c $(gen_dir)/s7_module.c $(gen_dir)/program.c


$(gen_dir)/program.c: $(bin_dir)/autoscheme-prime $(autoscheme_sources)
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile $(autoscheme_sources) -o $(gen_dir)/program.c --link-modules="auto_scheme_environment s7"



$(gen_dir)/s7_module.c: $(bin_dir)/autoscheme-prime $(mod_dir)/s7/module.scm
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime -m $(mod_dir)/s7/module.scm -n s7 -o $(gen_dir)/s7_module.c



$(gen_dir)/auto_scheme_environment_module.c: $(bin_dir)/autoscheme-prime $(mod_dir)/auto/scheme/environment/module.scm
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime -m $(mod_dir)/auto/scheme/environment/module.scm -n auto_scheme_environment -o $(gen_dir)/auto_scheme_environment_module.c








$(bin_dir)/autoscheme-prime: $(obj_dir)/s7.o $(prime_dir)/auto_scheme_environment_module.c $(prime_dir)/s7_module.c $(prime_dir)/program.c
	mkdir -p $(bin_dir)
	cc -I$(src_dir) -o $(bin_dir)/autoscheme-prime $(obj_dir)/s7.o $(prime_dir)/auto_scheme_environment_module.c $(prime_dir)/s7_module.c $(prime_dir)/program.c 




$(obj_dir)/s7.o: $(s7_dir)/s7.h $(s7_dir)/s7.c
	mkdir -p $(obj_dir)
	cc -o $(obj_dir)/s7.o -c $(s7_dir)/s7.c




install: $(bin_dir)/autoscheme
	cp $(bin_dir)/autoscheme $(install_dir)/

uninstall: 
	rm $(install_dir)/autoscheme

clean:
	rm -rf $(gen_dir)
	rm -rf $(obj_dir)
	rm -rf $(bin_dir)

	rm -rf tmp
