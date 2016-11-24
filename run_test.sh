#!/bin/sh
#set -eux

function run {
  v="$1"

  echo "$v" | cargo run -q > tmp.s
  if [ ! $? ]; then
    echo "Failed to compile $expr"
    exit
  fi

  gcc -o tmp.out test/driver.c tmp.s || exit
  result="`./tmp.out`"
  if [ "$result" != "$v" ]; then
    echo "Test failed: $v expected but got $result"
    exit
  fi
}

run 0
run 42

rm -f tmp.out tmp.s
echo "All tests passed"

