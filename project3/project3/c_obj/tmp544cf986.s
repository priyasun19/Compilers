	.align 4
	.text
.globl program
program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%edx
	pushl %edx
	pushl $27
	popl %ecx
	imull 0(%esp), %ecx
	movl %ecx,0(%esp)
	popl %eax
	popl %ebp
	ret
