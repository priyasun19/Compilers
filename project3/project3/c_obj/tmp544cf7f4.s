	.align 4
	.text
.globl program
program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%edx
	pushl $17
	cmpl 0(%esp),$0
	movl $0,0(%esp)
	sete 0(%esp)
	popl %ecx
	andl %ecx,0(%esp)
	popl %eax
	popl %ebp
	ret
