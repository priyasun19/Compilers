	.align 4
	.text
.globl program
program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%edx
	pushl %edx
	pushl $4
	movl 4(%esp),%ecx
	cmpl 0(%esp),%ecx
	movl $0,%ecx
	setl %cl
	addl $8,%esp
	pushl %ecx
	popl %eax
	popl %ebp
	ret
