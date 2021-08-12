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
	$(mod_dir)/auto/scheme/base/library.scm \
	$(mod_dir)/auto/scheme/eval/library.scm \
	$(mod_dir)/auto/scheme/load/library.scm \
	$(mod_dir)/auto/scheme/read/library.scm \
	$(mod_dir)/auto/scheme/repl/library.scm \
	$(mod_dir)/auto/scheme/cxr/library.scm \
	$(mod_dir)/auto/scheme/path/library.scm \
	$(mod_dir)/auto/scheme/write/library.scm \
	$(mod_dir)/auto/scheme/string/library.scm \
	$(mod_dir)/auto/scheme/args-fold/library.scm

autoscheme_sources =  \
	$(library_sources) \
	$(src_dir)/usage.scm \
	$(src_dir)/compiler.scm \
	$(src_dir)/interpreter.scm \
	$(src_dir)/repl.scm \
	$(src_dir)/program.scm 

all: build

build: $(bin_dir)/autoscheme



$(bin_dir)/autoscheme: $(obj_dir)/s7.o $(gen_dir)/mod_auto_scheme_environment_procedures.c $(gen_dir)/mod_auto_scheme_macros.c $(gen_dir)/mod_s7_library.c $(gen_dir)/autoscheme.c 
	mkdir -p $(bin_dir)
	cc $(strict_options) -I$(src_dir) -o $(bin_dir)/autoscheme $(obj_dir)/s7.o $(gen_dir)/mod_auto_scheme_environment_procedures.c $(gen_dir)/mod_auto_scheme_macros.c $(gen_dir)/mod_s7_library.c $(gen_dir)/autoscheme.c


$(gen_dir)/autoscheme.c: $(bin_dir)/autoscheme-prime $(autoscheme_sources)
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile $(autoscheme_sources) -o $(gen_dir)/autoscheme.c --link-modules="mod_auto_scheme_macros mod_auto_scheme_environment_procedures mod_s7_library"



$(gen_dir)/mod_s7_library.c: $(bin_dir)/autoscheme-prime $(mod_dir)/s7/library.scm
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime -m $(mod_dir)/s7/library.scm -n mod_s7_library -o $(gen_dir)/mod_s7_library.c



$(gen_dir)/mod_auto_scheme_environment_procedures.c: $(bin_dir)/autoscheme-prime $(mod_dir)/auto/scheme/environment/procedures.scm
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime -m $(mod_dir)/auto/scheme/environment/procedures.scm -n mod_auto_scheme_environment_procedures -o $(gen_dir)/mod_auto_scheme_environment_procedures.c


$(gen_dir)/mod_auto_scheme_macros.c: $(bin_dir)/autoscheme-prime $(mod_dir)/auto/scheme/macros.scm
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime -m $(mod_dir)/auto/scheme/macros.scm -n mod_auto_scheme_macros -o $(gen_dir)/mod_auto_scheme_macros.c






$(bin_dir)/autoscheme-prime: $(obj_dir)/s7.o $(prime_dir)/mod_auto_scheme_environment_procedures.c $(prime_dir)/mod_auto_scheme_macros.c $(prime_dir)/mod_s7_library.c $(prime_dir)/autoscheme.c
	mkdir -p $(bin_dir)
	cc -I$(src_dir) -o $(bin_dir)/autoscheme-prime $(obj_dir)/s7.o $(prime_dir)/mod_auto_scheme_environment_procedures.c $(prime_dir)/mod_auto_scheme_macros.c $(prime_dir)/mod_s7_library.c $(prime_dir)/autoscheme.c 




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
