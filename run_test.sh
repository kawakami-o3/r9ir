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
  gcc -o tmp.out test/driver.c tmp.s -undefined dynamic_lookup
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

testast '1' '1'
testast '(+ (- (+ 1 2) 3) 4)' '1+2-3+4'

test 0 0
test abc '"abc"'

test 3 '1+2'
test 3 '1 + 2'
test 10 '1+2+3+4'
test 4 '1+2-3+4'

testfail '"abc'
testfail '0abc'
testfail '1+'
testfail '1+"abc"'

echo "All tests passed"

