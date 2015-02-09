	.align 4
	.text
.globl program
program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%edx
	pushl $17
	pushl %edx
	popl %ecx
	shll %cl, 0(%esp)
	popl %eax
	popl %ebp
	ret
