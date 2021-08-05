project_dir = ../..
src_dir = $(project_dir)/src
prime_dir = $(src_dir)/prime
s7_dir = $(src_dir)/s7
lib_dir = $(src_dir)/libraries
gen_dir = $(project_dir)/gen
bin_dir = bin
obj_dir = _obj

install_dir = /usr/local/bin

strict_options = -Wall -Wextra -pedantic -Wmissing-prototypes -Wstrict-prototypes -Wold-style-definition -std=c89 

autoscheme_lib_sources = \
	$(lib_dir)/auto/scheme/base/definition.scm \
	$(lib_dir)/auto/scheme/eval/definition.scm \
	$(lib_dir)/scheme/load/definition.scm \
	$(lib_dir)/scheme/read/definition.scm \
	$(lib_dir)/scheme/repl/definition.scm \
	$(lib_dir)/scheme/cxr/definition.scm \
	$(lib_dir)/auto/scheme/write/definition.scm \
	$(lib_dir)/auto/srfi/13/definition.scm \
	$(lib_dir)/srfi/37/definition.scm

autoscheme_sources =  \
	$(autoscheme_lib_sources) \
	$(src_dir)/version.scm \
	$(src_dir)/usage.scm \
	$(src_dir)/compiler.scm \
	$(src_dir)/interpreter.scm \
	$(src_dir)/repl.scm \
	$(src_dir)/autoscheme.scm 

all: build

build: $(bin_dir)/autoscheme



$(bin_dir)/autoscheme: $(obj_dir)/s7.o $(gen_dir)/mod_scheme.c $(gen_dir)/autoscheme.c 
	mkdir -p $(bin_dir)
	cc $(strict_options) -I$(src_dir) -o $(bin_dir)/autoscheme $(obj_dir)/s7.o $(gen_dir)/mod_scheme.c $(gen_dir)/autoscheme.c


$(gen_dir)/autoscheme.c: $(bin_dir)/autoscheme-prime $(autoscheme_sources)
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile $(autoscheme_sources) -o $(gen_dir)/autoscheme.c -l mod_scheme


$(gen_dir)/mod_scheme.c: $(bin_dir)/autoscheme-prime $(src_dir)/scheme.scm
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime -m $(src_dir)/scheme.scm -n mod_scheme -o $(gen_dir)/mod_scheme.c



$(bin_dir)/autoscheme-prime: $(obj_dir)/s7.o $(prime_dir)/mod_scheme.c $(prime_dir)/autoscheme.c
	mkdir -p $(bin_dir)
	cc -I$(src_dir) -o $(bin_dir)/autoscheme-prime $(obj_dir)/s7.o $(prime_dir)/mod_scheme.c $(prime_dir)/autoscheme.c 




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

	# rm -rf tmp
