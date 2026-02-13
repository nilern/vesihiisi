	.intel_syntax noprefix

	.text

	.p2align 4
	.globl	callForeign
	.type	callForeign, @function
# uint64_t callForeign(
#	void (*f)(), bool fRet, uint8_t const* unboxings, ORef const* args, size_t argc)
callForeign:
	.cfi_startproc
	endbr64
	push rbp
	push r12
	push r13
	push r14
	push r15
	push rsi		# `returnsFlonum`
	push rdi		# The foreign function to be called
	mov rbp, rdx	# `unboxings`
	mov r10, rcx	# `args`
	mov r11, r8		# `argc`
	xor rax, rax	# `al` counts flonums, `ah` other arguments

.macro jmpFlo reg, dest
	cmp \reg, r12
	je \dest		# actual NaN?
	mov r15, \reg
	not r15
	test r15, r12
	jne \dest		# Non-NaN flonum?
.endm

.macro marshal reg, i, nonFixnum, unboxed
	mov r15, \reg	# Copy for testing fixnumness
	and \reg, r13	# Strip tag bits
	not r15
	test r15, r14
	jne \nonFixnum
# Sign extension to i64:
	sal \reg, 16
	sar \reg, 16
\nonFixnum:
	test BYTE PTR ((\i + 1) / 8)[rbp], 1 << ((\i + 1) % 8)
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
	movabs r13, 281474976710655	# `payloadMask`
	movabs r14, 9222527611924643840	# `fixnumTag`
	mov rdi, QWORD PTR [r10]		# `rdi = args[0]`
	jmpFlo rdi, .arg0f
# Non-flonum:
	marshal rdi, 0, .arg0NonFixnum, .arg0Unboxed
	jmp .arg1
.arg0f:
	marshalFlo xmm0, rdi

.arg1: # `args[1]`
	cmp r8, 1
	je .doCallForeign
	mov rsi, QWORD PTR 8[r10]
	jmpFlo rsi, .arg1f
# Non-flonum:
	test ah, ah
	je .iArg0
	marshal rsi, 1, .arg1NonFixnum1, .arg1Unboxed1
	jmp .arg2
.iArg0:
	mov rdi, rsi
	marshal rdi, 1, .arg1NonFixnum0, .arg1Unboxed0
	jmp .arg2
.arg1f:
	test al, al
	jne .fArg1
	marshalFlo xmm0, rsi
	jmp .arg2
.fArg1:
	marshalFlo xmm1, rsi

.arg2:
# TODO: More than 2 arguments

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
	pop r15
	pop r14
	pop r13
	pop r12
	pop rbp
	ret
	.cfi_endproc

	.section .note.GNU-stack,"",@progbits
