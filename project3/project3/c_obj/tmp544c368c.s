	.align 4
	.text
.globl _program
_program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%ecx
	pushl %ecx
	popl %eax
	ret
