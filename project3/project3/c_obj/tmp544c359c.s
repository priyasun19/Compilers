	.align 4
	.text
.globl _program
_program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%ecx
	pushl %ecx
	pushl $17
	movl 4(%esp),%edx
	addl 0(%esp),%edx
	addl $8,%esp
	pushl %edx
	pushl %ecx
	movl 4(%esp),%edx
	andl 0(%esp),%edx
	addl $8,%esp
	pushl %edx
