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
	cmpl %ecx,0(%esp)
	movl $0,0(%esp)
	setne 0(%esp)
	popl %eax
	popl %ebp
	ret
