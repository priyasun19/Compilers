	.align 4
	.text
.globl _program
_program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%ecx
	pushl $27
	pushl %ecx
	movl 4(%esp),%edx
	imull 0(%esp), %edx
	addl $8,%esp
	pushl %edx
