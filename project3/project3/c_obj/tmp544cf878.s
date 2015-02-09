	.align 4
	.text
.globl program
program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%edx
	pushl %edx
	pushl $4
	popl %ecx
	cmpl %ecx,0(%esp)
	movl $0,0(%esp)
	setle 0(%esp)
	popl %eax
	popl %ebp
	ret
