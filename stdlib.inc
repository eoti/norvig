.altmacro

.macro .plus. a b
	.if a != %rax
	movq a, %rax
	.endif
	addq b
.endm
