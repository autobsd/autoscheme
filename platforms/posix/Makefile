project_dir = ../..
src_dir = $(project_dir)/src
prime_dir = $(src_dir)/prime
s7_dir = $(src_dir)/s7
lib_dir = $(src_dir)/libraries
gen_dir = $(project_dir)/gen
bin_dir = bin

obj_dir = _obj

strict_options = -Wall -Wextra -pedantic -Wmissing-prototypes -Wstrict-prototypes -Wold-style-definition -std=c89 

autoscheme_lib_sources = \
	$(lib_dir)/auto/scheme/base/definition.scm \
	$(lib_dir)/scheme/load/definition.scm \
	$(lib_dir)/scheme/read/definition.scm \
	$(lib_dir)/auto/scheme/write/definition.scm \
	$(lib_dir)/auto/srfi/13/definition.scm \
	$(lib_dir)/srfi/37/definition.scm

autoscheme_sources = $(src_dir)/scheme.scm $(autoscheme_lib_sources) $(src_dir)/version.scm $(src_dir)/compiler.scm $(src_dir)/interpreter.scm $(src_dir)/autoscheme.scm 

all: build


build: $(bin_dir)/autoscheme




$(bin_dir)/autoscheme: $(gen_dir)/autoscheme.c 
	mkdir -p $(bin_dir)
	cc $(strict_options) -I$(src_dir) -o $(bin_dir)/autoscheme _obj/s7.o $(gen_dir)/autoscheme.c



$(gen_dir)/autoscheme.c: $(bin_dir)/autoscheme-prime $(autoscheme_sources)
	mkdir -p $(gen_dir)
	$(bin_dir)/autoscheme-prime --compile $(autoscheme_sources) -o $(gen_dir)/autoscheme.c



$(bin_dir)/autoscheme-prime: $(obj_dir)/autoscheme-prime.o $(obj_dir)/s7.o
	mkdir -p $(bin_dir)
	cc -o $(bin_dir)/autoscheme-prime -lm $(obj_dir)/s7.o $(obj_dir)/autoscheme-prime.o


$(obj_dir)/autoscheme-prime.o: $(prime_dir)/autoscheme.c
	mkdir -p $(obj_dir)
	cc $(strict_options) -I$(src_dir) -o $(obj_dir)/autoscheme-prime.o -c $(prime_dir)/autoscheme.c



$(obj_dir)/s7.o: $(s7_dir)/s7.h $(s7_dir)/s7.c
	mkdir -p $(obj_dir)
	cc -o $(obj_dir)/s7.o -c $(s7_dir)/s7.c



clean:
	rm -rf $(gen_dir)
	rm -rf $(obj_dir)
	rm -rf $(bin_dir)

	rm -rf tmp
