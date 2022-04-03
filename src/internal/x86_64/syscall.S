/*
	musl-tcc
	File:/src/internal/x86_64/syscall.S
	Copyright(c) 2022 ziyao.
*/

	.global		__syscall4,__syscall5,__syscall6

/*
	long int __syscall4(long int n, long int a1,long int a2,
			    long int a2,long int a3,long int a4);
	n	%rdi
	a1	%rsi
	a2	%rdx
	a3	%rcx
	a4	%r8
*/

	.type	__syscall4,@function
__syscall4:
	movq	%rdi,	%rax
	movq	%rsi,	%rdi
	movq	%rdx,	%rsi
	movq	%rcx,	%rdx
	movq	%r8,	%r10
	syscall
	retq

	.type	__syscall5,@function
__syscall5:
	movq	%rdi,	%rax
	movq	%rsi,	%rdi
	movq	%rdx,	%rsi
	movq	%rcx,	%rdx
	movq	%r8,	%r10
	movq	%r9,	%r8
	syscall
	retq

	.type	__syscall6,@function
__syscall6:
	movq	%rdi,	%rax
	movq	%rsi,	%rdi
	movq	%rdx,	%rsi
	movq	%rcx,	%rdx
	movq	%r8,	%r10
	movq	%r9,	%r8
	movq	(%rsp),	%r9		// The last argument is stored on
					// the stack
					// But do NOT mess up the stack
	syscall
	retq
