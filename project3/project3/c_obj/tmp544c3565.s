	.align 4
	.text
.globl _program
_program:
	pushl %ecx
	movl 8(%esp),%ecx
	movl %esp,%ebp
	pushl %ebp
