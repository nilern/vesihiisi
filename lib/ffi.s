	.intel_syntax noprefix

	.text

	.p2align 4
	.globl	callForeign
	.type	callForeign, @function
# uint64_t callForeign(
#	void (*f)(..), bool fRet, uint8_t const* unboxings, ORef const* args, size_t argc)
callForeign:
	.cfi_startproc
	endbr64
	push rbp
	push r12
	push r13
	push r14
	push rsi		# `returnsFlonum`
	push rdi		# The foreign function to be called
	mov rbp, rdx	# `unboxings`
	mov r10, rcx	# `args`
	mov r11, r8		# `argc`
	xor rax, rax	# `al` counts flonums, `ah` other arguments

.macro jmpFlo reg, dest
	cmp \reg, r12
	je \dest		# actual NaN?
	not \reg
	test \reg, r12
	jne \dest		# Non-NaN flonum?
.endm

.macro marshal reg, i, unboxed
	and \reg, r13
	test BYTE PTR \i[rbp], 1 << \i
	je \unboxed					# Does not need unboxing load?
	mov \reg, QWORD PTR [\reg]
\unboxed:
	inc ah
.endm

.macro marshalFlo reg, ireg
	movq \reg, \ireg
	inc al
.endm

# `args[0]`:
	test r8, r8
	je .doCallForeign				# `argc == 0` => done marshalling
	movabs r12, 9222246136947933184	# `nonFlonumTag`
	mov rdi, QWORD PTR [r10]		# `rdi = args[0]`
	jmpFlo rdi, .arg0f
# Non-flonum:
	movabs r13, 281474976710655	# `payloadMask`
	marshal rdi, 0, .arg0Unboxed
	jmp .arg1
.arg0f:
	marshalFlo xmm0, rdi

arg1: # `args[1]`
	cmp r8, 1
	je .doCallForeign
	mov rsi, QWORD PTR 1[r10]
	jmpFlo rsi, .arg1f
# Non-flonum:
	movabs r13, 281474976710655
	test ah, ah
	je .iArg0
	marshal rsi, 1, .arg1Unboxed1
	jmp .arg2
.iArg0:
	mov rdi, rsi
	marshal rdi, 1, .arg1Unboxed0
	jmp .arg2
.arg1f:
	marshalFlo xmm0, rdi

.arg2:

# Finally, the foreign function call itself:
.doCallForeign:
	pop r11
	call r11

# If (supposedly) returned flonum, return the bits in `rax` instead:
	pop r11
	test r11, r11
	je .done
	movq rax, xmm0

.done:
	pop r14
	pop r13
	pop r12
	pop rbp
	ret
	.cfi_endproc
