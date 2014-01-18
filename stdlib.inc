.intel_syntax noprefix
.altmacro


.macro .PLUS a b
	.if \a != rax
		mov rax, \a
	.endif	
	add rax, \b
.endm
