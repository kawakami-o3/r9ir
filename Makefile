
all: build

build:
	cargo build

test:
	#gcc -E -C -P test/test.c > tmp-test.tmp
	#RUST_BACKTRACE=1 cargo run -q tmp-test.tmp > tmp-test.s
	gcc -E -C -P test/test.c | RUST_BACKTRACE=1 cargo run -q - > tmp-test1.s
	#RUST_BACKTRACE=1 cargo run -q test/token.c > tmp-test2.s
	echo 'int global_arr[1] = {5};' | gcc -xc -c -o tmp-test2.o -
	#gcc -static -o tmp-test tmp-test1.s tmp-test2.s tmp-test2.o
	gcc -static -o tmp-test tmp-test1.s tmp-test2.o
	./tmp-test

dump-node:
	gcc -E -C -P test/test.c > tmp-test.tmp
	RUST_BACKTRACE=1 cargo run -q -- -dump-node tmp-test.tmp > tmp-test.s

dump-ir1:
	gcc -E -C -P test/test.c > tmp-test.tmp
	RUST_BACKTRACE=1 cargo run -q -- -dump-ir1 tmp-test.tmp > tmp-test.s

dump-ir2:
	gcc -E -C -P test/test.c > tmp-test.tmp
	RUST_BACKTRACE=1 cargo run -q -- -dump-ir2 tmp-test.tmp > tmp-test.s

clean:
	cargo clean
	rm -f tmp-test

.PHONY: all build test clean
