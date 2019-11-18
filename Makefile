CC := RUST_BACKTRACE=1 cargo run --bin r9ir -q
#CC := cargo run -q

all: build

build:
	cargo build --bin r9ir
	cargo build --bin vm

test-unit:
	@$(CC) test/test.c > tmp-test1.s
	@gcc -xc -c -o tmp-test2.o test/gcc.c
	@gcc -static -o tmp-test1 tmp-test1.s tmp-test2.o
	@./tmp-test1

test-include:
	@$(CC) test/token.c > tmp-test2.s
	@gcc -static -o tmp-test2 tmp-test2.s
	@./tmp-test2

test: test-unit test-include

dump-node:
	@#gcc -E -C -P test/test.c > tmp-test.tmp
	@#RUST_BACKTRACE=1 cargo run -q -- -dump-node tmp-test.tmp > tmp-test.s
	@RUST_BACKTRACE=1 cargo run -q -- -dump-node test/test.c > tmp-test.s


dump-ir1:
	@#gcc -E -C -P test/test.c > tmp-test.tmp
	@#RUST_BACKTRACE=1 cargo run -q -- -dump-ir1 tmp-test.tmp > tmp-test.s
	@RUST_BACKTRACE=1 cargo run -q -- -dump-ir1 test/test.c > tmp-test.s

dump-ir2:
	@gcc -E -C -P test/test.c > tmp-test.tmp
	@RUST_BACKTRACE=1 cargo run -q -- -dump-ir2 tmp-test.tmp > tmp-test.s

clean:
	cargo clean
	rm -f tmp-test1 tmp-test2 tmp-test1.s tmp-test2.s tmp-test1.o tmp-test2.o

.PHONY: all build test clean
