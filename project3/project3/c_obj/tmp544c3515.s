	.align 4
	.text
.globl _program
_program:
	pushl %edx
	addl $8,%esp
	orl 0(%esp),%edx
	movl 4(%esp),%edx
	pushl %ecx
	pushl $17
	movl 8(%esp),%ecx
	movl %esp,%ebp
	pushl %ebp
