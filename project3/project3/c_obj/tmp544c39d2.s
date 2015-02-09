	.align 4
	.text
.globl program
program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%ecx
	pushl $17
	pushl %ecx
	movl 4(%esp),%edx
