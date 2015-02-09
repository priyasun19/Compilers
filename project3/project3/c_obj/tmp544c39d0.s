	.align 4
	.text
.globl program
program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%ecx
	pushl %ecx
	pushl %ecx
	movl 4(%esp),%edx
	orl 0(%esp),%edx
	addl $8,%esp
	pushl %edx
	popl %eax
	popl %ebp
	ret
