#include<stdio.h>

int main() {
	printf("a: %d \\%03o\n", '\a', '\a');
	printf("b: %d \\%03o\n", '\b', '\b');
	printf("f: %d \\%03o\n", '\f', '\f');
	printf("n: %d \\%03o\n", '\n', '\n');
	printf("r: %d \\%03o\n", '\r', '\r');
	printf("t: %d \\%03o\n", '\t', '\t');
	printf("v: %d \\%03o\n", '\v', '\v');
	printf("e: %d \\%03o\n", '\e', '\e');
	printf("E: %d \\%03o\n", '\E', '\E');
}


