#!/bin/sh
set -x

run() {
	RUST_BACKTRACE=1 cargo run -q "$1"
}

#run 'return 5+20-4;'
#run 'return plus(2, 3);'
#run 'int main() { return 1; }'
#run 'main() { return 2*3+4; }'
#run 'one() { return 1; } main() { return one(); }'
#run 'main() { sum=0; for (i=10; i<15; i=i+1) sum = sum + i; return sum;}'
run 'int main() { int i=1; int j=1; int k; for (k=0; k<10; k=k+1) { int m=i+j; i=j; j=m; } return i;}'

#cat tmp.s

#gcc -g3 -o tmp.out test/driver.c tmp.s -undefined dynamic_lookup

#gcc -g3 -o tmp.out test/driver.c tmp.s && ./tmp.out
