#!/bin/sh
set -x

function run_compiler {
  RUST_BACKTRACE=1 cargo run -q -- $1
}

function compile {
  echo "$1" | run_compiler > tmp.s
  if [ $? -ne 0 ]; then
    echo "Failed to compile $1"
    exit
  fi
  #gcc -o tmp.out test/driver.c tmp.s -undefined dynamic_lookup
  gcc -g -o tmp.out test/driver.c tmp.s
  #gcc -o tmp.out test/driver.c tmp.s
  if [ $? -ne 0 ]; then
    echo "GCC failed"
    exit
  fi
}

function assertequal {
  if [ "$1" != "$2" ]; then
    echo "Test faild: $2 expected but got $1"
    exit
  fi
}

function testast {
  result="$(echo "$2" | run_compiler -a)"
  if [ $? -ne 0 ]; then
    echo "Failed to compile $1"
    exit
  fi
  assertequal "$result" "$1"
}

function test {
  compile "$2"
  assertequal "$(./tmp.out)" "$1"
}

function testfail {
  expr="$1"
  echo "$expr" | run_compiler > /dev/null 2>&1
  #echo "$expr" | run_compiler
  if [ $? -eq 0 ]; then
    echo "Should fail to compile, but succeded: $expr"
    exit
  fi
}

cargo build

# Parser
testast '1' '1;'
testast '(+ (- (+ 1 2) 3) 4)' '1+2-3+4;'
testast '(+ (+ 1 (* 2 3)) 4)' '1+2*3+4;'
testast '(+ (* 1 2) (* 3 4))' '1*2+3*4;'
testast '(+ (/ 4 2) (/ 6 3))' '4/2+6/3;'
testast '(/ (/ 24 2) 4)' '24/2/4;'

testast '(decl int a 3)' 'int a=3;'
testast "(decl char c 'a')" "char c='a';"
testast '(decl int a 1)(decl int b 2)(= a (= b 3))' 'int a=1;int b=2;a=b=3;'

testast '"abc"' '"abc";'
testast "'c'" "'c';"

testast 'a()' 'a();'
#testast 'a(1,2,3,4,5,6)' 'a(1,2,3,4,5,6);'

# Expression
test 0 '0;'

test 3 '1+2;'
test 3 '1 + 2;'
test 10 '1+2+3+4;'
test 11 '1+2*3+4;'
test 14 '1*2+3*4;'
test 4 '4/2+6/3;'
test 3 '24/2/4;'

test 98 "'a'+1;"

test 2 '1;2;'
test 3 'int a=1;a+2;'
test 102 'int a=1;int b=48+2;int c=a+b;c*2;'

test 25 'sum2(20, 5);'
#test 15 'sum5(1,2,3,4,5);'
test a3 'printf("a");3;'
test abc5 'printf("%s", "abc");5;'
test b1 "printf(\"%c\", 'a'+1);1;"

testfail '0abc;'
testfail '1+;'
testfail '1=2;'

# Incompatible type
testfail '"a"+1;'

echo "All tests passed"

