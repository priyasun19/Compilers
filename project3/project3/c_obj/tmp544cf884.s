	.align 4
	.text
.globl program
program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%edx
	pushl %edx
	pushl %edx
	popl %ecx
	addl %ecx,0(%esp)
	pushl %edx
	popl %ecx
	imull 0(%esp), %ecx
	movl %ecx,0(%esp)
	popl %eax
	popl %ebp
	ret
