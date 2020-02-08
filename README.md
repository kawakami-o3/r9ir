# r9ir

[![Build Status](https://travis-ci.org/kawakami-o3/r9ir.svg?branch=master)](https://travis-ci.org/kawakami-o3/r9ir)


This project is influenced by rui314/9cc.

# Installation

```
% cargo install r9ir
```

# Usage

```
% cat hello.c
void printf();

int main()
{
  printf("hello world");
}
% r9ir hello.c > hello.s
% gcc hello.s
% ./a.out
Hello world
```

In this example, `hello.s` is as follows,

```
.intel_syntax noprefix
.data
.L.str1:
	.ascii "hello world\000"
.text
.global main
main:
	push rbp
	mov rbp, rsp
	sub rsp, 0
	push r12
	push r13
	push r14
	push r15
.L2:
	jmp .L3
.L3:
	lea r10, .L.str1
	mov rdi, r10
	push r10
	push r11
	mov rax, 0
	call printf
	pop r11
	pop r10
	mov r10, rax
	mov rax, r10
	jmp .Lend4
	mov r10, 0
.Lend4:
	pop r15
	pop r14
	pop r13
	pop r12
	mov rsp, rbp
	pop rbp
	ret
```

