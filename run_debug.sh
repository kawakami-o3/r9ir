#!/bin/sh
set -x

run() {
	RUST_BACKTRACE=1 cargo run -q "$1"
}

#run 'int main() { return 1; }'
#run 'int main() { if (1) return 2; return 3; }'
#run 'int main() { int sum=0; int i; for (i=10; i<15; i=i+1) sum = sum + i; return sum;}'
#run 'int main() { int i=1; int j=1; int k; for (k=0; k<10; k=k+1) { int m=i+j; i=j; j=m; } return i;}'

#run 'int mul(int a, int b) { return a * b; } int main() { return mul(2, 3); }'
#run 'int add(int a, int b, int c, int d, int e, int f) { return a+b+c+d+e+f; } int main() { return add(1,2,3,4,5,6); }'

#run 'int main() { int *p = alloc1(3, 5); return *p + *(p + 1); }'
#run 'int main() { int *p = alloc2(2, 7); return *p + *(p - 1); }'
#run 'int main() { int **p = alloc_ptr_ptr(2); return **p; }'
#run 'int main() { int ary[2]; *ary=1; *(ary+1)=2; return *ary + *(ary+1);}'
#run 'int main() { int x[4]; return sizeof x; }'
#run 'int main() { char x; return sizeof(x); }'

#run 'int main() { char *p = "abc"; return p[0]; }'
#run 'int main() { char *p = "abc"; return p[1]; }'
#run 'int main() { char *p = "abc"; return p[2]; }'
#run 'int main() { char *p = "abc"; return p[3]; }'

#run 'int main() { int x = 1; { int x = 2; } return x; }'

run 'int x; int main() { return x; }'
#run 'int x; int main() { x = 5; return x; }'
#run 'int x[5]; int main() { return sizeof(x); }'
#run 'int x[5]; int main() { x[0] = 5; x[4] = 10; return x[0] + x[4]; }'

#cat tmp.s

#gcc -g3 -o tmp.out test/driver.c tmp.s -undefined dynamic_lookup

#gcc -g3 -o tmp.out test/driver.c tmp.s && ./tmp.out
