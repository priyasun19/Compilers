	.align 4
	.text
.globl program
program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%ecx
	pushl $27
	pushl %ecx
	movl 4(%esp),%edx
	imull 0(%esp), %edx
	addl $8,%esp
	pushl %edx
	popl %eax
	popl %ebp
	ret
