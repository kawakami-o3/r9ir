#!/bin/sh
#cargo run -q "$(gcc -E -P test/test.c)" 


env RUST_BACKTRACE=1 cargo run "$(gcc -E -P test/test.c)" 


