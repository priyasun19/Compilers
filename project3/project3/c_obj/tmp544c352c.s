	.align 4
	.text
.globl _program
_program:
	pushl %edx
	addl $8,%esp
	orl 0(%esp),%edx
	movl 4(%esp),%edx
	pushl $42
	pushl %edx
	addl $8,%esp
	andl 0(%esp),%edx
	movl 4(%esp),%edx
	pushl $17
	pushl %ecx
	movl 8(%esp),%ecx
	movl %esp,%ebp
	pushl %ebp
