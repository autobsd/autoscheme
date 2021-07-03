project_dir = ../..
src_dir = $(project_dir)/src
s7_dir = $(src_dir)/s7
bin_dir = bin

obj_dir = _obj

strict_options = -Wall -Wextra -std=c89 -pedantic -Wmissing-prototypes -Wstrict-prototypes -Wold-style-definition

all: build

build: $(obj_dir)/main.o $(obj_dir)/s7.o

	mkdir -p $(bin_dir)
	cc $(strict_options) -o $(bin_dir)/autoscheme -lm $(obj_dir)/s7.o $(obj_dir)/main.o


$(obj_dir)/main.o: $(src_dir)/version.h $(src_dir)/main.c
	mkdir -p $(obj_dir)
	cc -std=c89 -I$(src_dir)/s7 -o $(obj_dir)/main.o -c $(src_dir)/main.c

$(obj_dir)/s7.o: $(s7_dir)/s7.h $(s7_dir)/s7.c
	mkdir -p $(obj_dir)
	cc -o $(obj_dir)/s7.o -c $(s7_dir)/s7.c