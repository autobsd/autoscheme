project_dir = ../..
src_dir = $(project_dir)/src
s7_dir = $(src_dir)/s7
bin_dir = bin

obj_dir = _obj

strict_options = -Wall -Wextra -pedantic -Wmissing-prototypes -Wstrict-prototypes -Wold-style-definition -std=c89 

all: build





build: $(bin_dir)/autoscheme




$(bin_dir)/autoscheme: $(src_dir)/autoscheme.scm.c 
	cc $(strict_options) -IIsrc -o $(bin_dir)/autoscheme _obj/s7.o $(src_dir)/autoscheme.scm.c



$(src_dir)/autoscheme.scm.c: $(bin_dir)/autoscheme-prime $(src_dir)/autoscheme.scm
	$(bin_dir)/autoscheme-prime -i $(src_dir)/compiler.scm $(src_dir)/autoscheme.scm



$(bin_dir)/autoscheme-prime: $(obj_dir)/autoscheme-prime.o $(obj_dir)/s7.o

	mkdir -p $(bin_dir)
	cc -o $(bin_dir)/autoscheme-prime -lm $(obj_dir)/s7.o $(obj_dir)/autoscheme-prime.o


$(obj_dir)/autoscheme-prime.o: $(src_dir)/autoscheme-prime.c
	mkdir -p $(obj_dir)
	cc $(strict_options) -I$(src_dir)/s7 -o $(obj_dir)/autoscheme-prime.o -c $(src_dir)/autoscheme-prime.c

$(obj_dir)/s7.o: $(s7_dir)/s7.h $(s7_dir)/s7.c
	mkdir -p $(obj_dir)
	cc -o $(obj_dir)/s7.o -c $(s7_dir)/s7.c


clean:
	rm $(src_dir)/autoscheme.scm.c