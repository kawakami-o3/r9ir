
all: build

build:
	cargo build

test:
	gcc -E -C -P test/test.c > tmp-test.tmp
	cargo run -q tmp-test.tmp > tmp-test.s
	echo 'int global_arr[1] = {5};' | gcc -xc -c -o tmp-test2.o -
	gcc -static -o tmp-test tmp-test.s tmp-test2.o
	./tmp-test

clean:
	cargo clean
	rm -f tmp-test

.PHONY: all build test clean
