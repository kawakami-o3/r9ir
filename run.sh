#!/bin/sh
	
#cargo run -q "$(gcc -E -P test/test.c)" 
cargo run "$(gcc -E -P test/test.c)" 


