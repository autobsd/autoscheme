project_dir = ../../
src_dir = $(project_dir)src/
bin_dir = bin/


all: build

build: $(src_dir)main.c

	mkdir -p $(bin_dir)
	cc -I$(src_dir)s7 -o $(bin_dir)autoscheme -lm $(src_dir)s7/s7.c $(src_dir)main.c


