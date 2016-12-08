#!/bin/sh
#set -eux

function run {
  v="$1"
  expect="$2"

  echo "$v" | cargo run -q > tmp.s
  if [ ! $? ]; then
    echo "Failed to compile $expr"
    exit
  fi

  gcc -o tmp.out test/driver.c tmp.s -undefined dynamic_lookup || exit
  result="`./tmp.out`"
  if [ "$result" != "$expect" ]; then
    echo "Test failed: $expect expected but got $result"
    exit
  fi
}

run 0 0
run 42 42
run '"abc"' abc
run '"ab---c"' ab---c

rm -f tmp.out tmp.s
echo "All tests passed"

