#!/bin/sh
set -x
#echo '1+2-3' | RUST_BACKTRACE=1 cargo run -q -- -a
#echo 'printf("a");3;' | RUST_BACKTRACE=1 cargo run -q -- > tmp.s

echo 'int a=61;int *b=&a;*b;' | RUST_BACKTRACE=1 cargo run -q -- > tmp.s

#cat tmp.s

#gcc -g3 -o tmp.out test/driver.c tmp.s -undefined dynamic_lookup

gcc -g3 -o tmp.out test/driver.c tmp.s && ./tmp.out
