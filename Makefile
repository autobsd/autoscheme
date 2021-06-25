version_tag = $$(git describe)

all: build

build: src/main.c
	mkdir -p bin
	cc -DVERSION=\"$(version_tag)\" src/main.c -o bin/scheme


run:
	bin/scheme -V