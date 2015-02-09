	.align 4
	.text
.globl program
program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%ecx
	popl %eax
	popl %ebp
	ret
