	.align 4
	.text
.globl program
program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%ecx
	pushl %ecx
	pushl $4
	movl 4(%esp),%edx
	cmpl 0(%esp),%edx
	movl $0,%edx
	setl %dl
	addl $8,%esp
	pushl %edx
	popl %eax
	popl %ebp
	ret
