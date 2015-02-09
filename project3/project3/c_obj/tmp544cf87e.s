	.align 4
	.text
.globl program
program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%edx
	pushl $515
	pushl %edx
	popl %ecx
	shrl %cl, 0(%esp)
	popl %eax
	popl %ebp
	ret
