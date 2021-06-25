version_tag = $$(git describe)

all: build

build: src/main.c
	mkdir -p bin
	cc -DVERSION=\"$(version_tag)\" src/s7/s7.c src/main.c  -Isrc/s7 -o bin/scheme


run:
	bin/scheme -r