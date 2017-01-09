#!/bin/sh
set -x
#echo '1+2-3' | RUST_BACKTRACE=1 cargo run -q -- -a
echo '1+2' | RUST_BACKTRACE=1 cargo run -q -- > tmp.s

gcc -o tmp.out test/driver.c tmp.s -undefined dynamic_lookup

./tmp.out
