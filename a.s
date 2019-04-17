	.text
	.file	"<string>"
	.globl	ctop                    # -- Begin function ctop
	.p2align	4, 0x90
	.type	ctop,@function
ctop:                                   # @ctop
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	(%rsi), %edi
	callq	exit
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	ctop, .Lfunc_end0-ctop
	.cfi_endproc
                                        # -- End function
	.globl	f1.1                    # -- Begin function f1.1
	.p2align	4, 0x90
	.type	f1.1,@function
f1.1:                                   # @f1.1
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	(%rsi), %rax
	movq	8(%rsi), %rdi
	movq	$f3.2, 8(%rsp)
	movq	%rsp, %rcx
	movq	%rcx, 16(%rsp)
	leaq	8(%rsp), %rsi
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	jmpq	*%rax                   # TAILCALL
.Lfunc_end1:
	.size	f1.1, .Lfunc_end1-f1.1
	.cfi_endproc
                                        # -- End function
	.globl	f3.2                    # -- Begin function f3.2
	.p2align	4, 0x90
	.type	f3.2,@function
f3.2:                                   # @f3.2
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	(%rdx), %rax
	movq	8(%rdx), %rdi
	xorl	%edx, %edx
	popq	%rcx
	.cfi_def_cfa_offset 8
	jmpq	*%rax                   # TAILCALL
.Lfunc_end2:
	.size	f3.2, .Lfunc_end2-f3.2
	.cfi_endproc
                                        # -- End function
	.globl	f5.3                    # -- Begin function f5.3
	.p2align	4, 0x90
	.type	f5.3,@function
f5.3:                                   # @f5.3
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	(%rdx), %rax
	movq	8(%rdx), %rdi
	movq	%rsi, 24(%rsp)
	movq	%rsi, 32(%rsp)
	movq	$f7.4, 8(%rsp)
	leaq	24(%rsp), %rcx
	movq	%rcx, 16(%rsp)
	leaq	8(%rsp), %rsi
	xorl	%edx, %edx
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	jmpq	*%rax                   # TAILCALL
.Lfunc_end3:
	.size	f5.3, .Lfunc_end3-f5.3
	.cfi_endproc
                                        # -- End function
	.globl	f7.4                    # -- Begin function f7.4
	.p2align	4, 0x90
	.type	f7.4,@function
f7.4:                                   # @f7.4
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	(%rsi), %r8
	movq	8(%rsi), %rax
	movq	(%rdi), %rcx
	movq	%rcx, 16(%rsp)
	movq	(%rdi), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rdx, 32(%rsp)
	movq	$f15.5, (%rsp)
	leaq	16(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rsp, %rdx
	movq	%rax, %rdi
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	jmpq	*%r8                    # TAILCALL
.Lfunc_end4:
	.size	f7.4, .Lfunc_end4-f7.4
	.cfi_endproc
                                        # -- End function
	.globl	f15.5                   # -- Begin function f15.5
	.p2align	4, 0x90
	.type	f15.5,@function
f15.5:                                  # @f15.5
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	(%rdi), %rcx
	movq	(%rcx), %r8
	movq	8(%rcx), %rax
	movq	%rcx, 24(%rsp)
	movq	16(%rdi), %rcx
	movq	%rcx, 32(%rsp)
	movq	$f9.6, 8(%rsp)
	leaq	24(%rsp), %rcx
	movq	%rcx, 16(%rsp)
	leaq	8(%rsp), %rdx
	movq	%rax, %rdi
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	jmpq	*%r8                    # TAILCALL
.Lfunc_end5:
	.size	f15.5, .Lfunc_end5-f15.5
	.cfi_endproc
                                        # -- End function
	.globl	f9.6                    # -- Begin function f9.6
	.p2align	4, 0x90
	.type	f9.6,@function
f9.6:                                   # @f9.6
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	(%rsi), %rcx
	movq	8(%rsi), %rax
	movq	(%rdi), %rdx
	movq	%rdx, (%rsp)
	movq	$f11.7, 8(%rsp)
	movq	%rsp, %rdx
	movq	%rdx, 16(%rsp)
	movq	8(%rdi), %rdx
	leaq	8(%rsp), %rsi
	movq	%rax, %rdi
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	jmpq	*%rcx                   # TAILCALL
.Lfunc_end6:
	.size	f9.6, .Lfunc_end6-f9.6
	.cfi_endproc
                                        # -- End function
	.globl	f11.7                   # -- Begin function f11.7
	.p2align	4, 0x90
	.type	f11.7,@function
f11.7:                                  # @f11.7
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$40, %rsp
	.cfi_def_cfa_offset 48
	movq	(%rsi), %rcx
	movq	8(%rsi), %rax
	movq	(%rdi), %rdi
	movq	%rdi, 24(%rsp)
	movq	%rdx, 32(%rsp)
	movq	$f13.8, 8(%rsp)
	leaq	24(%rsp), %rdx
	movq	%rdx, 16(%rsp)
	leaq	8(%rsp), %rdx
	movq	%rax, %rdi
	addq	$40, %rsp
	.cfi_def_cfa_offset 8
	jmpq	*%rcx                   # TAILCALL
.Lfunc_end7:
	.size	f11.7, .Lfunc_end7-f11.7
	.cfi_endproc
                                        # -- End function
	.globl	f13.8                   # -- Begin function f13.8
	.p2align	4, 0x90
	.type	f13.8,@function
f13.8:                                  # @f13.8
	.cfi_startproc
# %bb.0:                                # %entry
	pushq	%rax
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rax
	movq	8(%rdi), %rdx
	movq	(%rax), %rcx
	movq	8(%rax), %rdi
	popq	%rax
	.cfi_def_cfa_offset 8
	jmpq	*%rcx                   # TAILCALL
.Lfunc_end8:
	.size	f13.8, .Lfunc_end8-f13.8
	.cfi_endproc
                                        # -- End function
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:                                # %entry
	subq	$72, %rsp
	.cfi_def_cfa_offset 80
	movq	$f1.1, 56(%rsp)
	movl	$f1.1, %eax
	leaq	16(%rsp), %rdi
	movq	%rdi, 64(%rsp)
	movq	$f5.3, 40(%rsp)
	leaq	8(%rsp), %rcx
	movq	%rcx, 48(%rsp)
	movq	$ctop, 24(%rsp)
	movq	%rsp, %rcx
	movq	%rcx, 32(%rsp)
	leaq	40(%rsp), %rsi
	leaq	24(%rsp), %rdx
	callq	*%rax
	movl	(%rax), %eax
	addq	$72, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end9:
	.size	main, .Lfunc_end9-main
	.cfi_endproc
                                        # -- End function

	.section	".note.GNU-stack","",@progbits
