	.align 4
	.text
.globl program
program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%edx
	pushl $17
	pushl %edx
	movl 4(%esp),%ecx
	andl 0(%esp),%ecx
	addl $8,%esp
	pushl %ecx
	popl %eax
	popl %ebp
	ret
