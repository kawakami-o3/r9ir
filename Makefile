
all: build

build:
	cargo build

test:
	cargo run -q "$$(gcc -E -P test/test.c)" > tmp-test.s
	echo 'int global_arr[1] = {5};' | gcc -xc -c -o tmp-test2.o -
	gcc -static -o tmp-test tmp-test.s tmp-test2.o
	./tmp-test

clean:
	cargo clean

.PHONY: all build test clean
