	.align 4
	.text
.globl _program
_program:
	pushl %edx
	addl $8,%esp
	imull 0(%esp), %edx
	movl 4(%esp),%edx
	pushl $27
	pushl %ecx
	movl 8(%esp),%ecx
	movl %esp,%ebp
	pushl %ebp
