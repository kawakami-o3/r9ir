#!/bin/sh
set -x

run() {
	RUST_BACKTRACE=1 cargo run -q "$1"
}

#run 'int main() { return 1; }'
#run 'int main() { if (1) return 2; return 3; }'
#run 'int main() { int sum=0; int i; for (i=10; i<15; i=i+1) sum = sum + i; return sum;}'
#run 'int main() { int i=1; int j=1; int k; for (k=0; k<10; k=k+1) { int m=i+j; i=j; j=m; } return i;}'

run 'extern int global_arr[1]; int main() { return global_arr[0]; }'


#cat tmp.s

#gcc -g3 -o tmp.out test/driver.c tmp.s -undefined dynamic_lookup

#gcc -g3 -o tmp.out test/driver.c tmp.s && ./tmp.out
