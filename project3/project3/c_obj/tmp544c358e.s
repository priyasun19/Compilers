	.align 4
	.text
.globl _program
_program:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%esp),%ecx
	pushl %ecx
	pushl $4
	movl 4(%esp),%edx
	cmpl 0(%esp),%edx
	movl $0,%edx
	setge %dl
	addl $8,%esp
	pushl %edx
