.intel_syntax noprefix
.arch pentium
.altmacro

/*****************************************************************
						GENERAL MACROS
*****************************************************************/

/* Allocate len quads and put pointer to memory in reg */
.macro alloc reg len
    mov \reg, \len*8
    add \reg, brk
    xchg \reg, brk
.endm

/* Create a closure
   Layout: |function pointer|n|arg1|arg2|...|argn| */
.macro build_closure fun:req, args:vararg
    n = 0
    .irp arg \args
        .ifnb arg
            n = n + 1
        .endif
    .endr
    alloc rax (n+2)
    mov qword ptr [rax], offset(\fun)
    mov qword ptr [rax+8], n
    .if n>0
        i = 2*8
        .irp arg \args
             mov qword ptr [rax + i], arg
             i = i + 8
        .endr
    .endif
.endm

/* Push arguments on stack in reverse order */
.macro reverse_push args:vararg
	n=0
	.irp arg,\args
		.ifnc \arg,
    		n=n+1
		.endif
	.endr
	.if n>0
		i=n
		.irp junk,\args
    		j=0
    		.irp arg,\args
    			j=j+1
    			.if j==i
        			push arg
        			.exitm
    			.endif
    		.endr
    		i=i-1
		.endr
	.endif
.endm


/* Call a closure */
.macro call_closure fun:req, args:vararg
	mov rbp, \fun
	cmpq [rbp + 8], 0
	jg 1f
	cinvoke [rbp], args
	jmp 2f
	1:
	cmpq [rbp + 8], 1
	jg 1f
	cinvoke [rbp], [rbp+16], args
	jmp 2f
	1:
	cmpq [rbp + 8], 2
	jg 1f
	cinvoke [rbp], [rbp+16], [rbp+24], args
	jmp 2f
	1:
	cmpq [rbp + 8], 3
	jg 1f
	cinvoke [rbp], [rbp+16], [rbp+24], [rbp+32], args
	jmp 2f
	1:
	cmpq [rbp + 8], 4
	jg 1f
	cinvoke [rbp], [rbp+16], [rbp+24], [rbp+32], [rbp+40], args
	jmp 2f
	1:
	cmpq [rbp + 8], 5
	jg 1f
	cinvoke [rbp], [rbp+16], [rbp+24], [rbp+32], [rbp+40], [rbp+48], args
	jmp 2f
	1:
	cmpq [rbp + 8], 6
	jg 1f
	cinvoke [rbp], [rbp+16], [rbp+24], [rbp+32], [rbp+40], [rbp+48], [rbp+56], args
	jmp 2f
	1:
	reverse_push args
	mov rcx, [rbp+8]
	mov rdx, rcx
	add rdx, 1
	imul rdx, 8
	sub rcx, 6
	3:
	push [rbp + rdx]
	sub rdx, 8
	loop 3b
	mov rdi, [rbp+16]
	mov rsi, [rbp+24]
	mov rdx, [rbp+32]
	mov rcx, [rbp+40]
	mov r8,  [rbp+48]
	mov r9,  [rbp+56]
    mov    rax, 0
	call [rbp]
	mov rbp, [rbp+8]
	add rbp, i - 6
	imul rbp, 8
	add rsp, rbp
	2:
.endm

/* x64 ABI. Does not handle floating point - fix later */
.macro cinvoke fun:req args:vararg
	n = 0
	i = 0
	j = 0
	.irp arg \args
		.ifnb arg
			n = n + 1
		.endif
	.endr
	.irp arg \args
		.ifnb arg
			i = i + 1
			.if i==1
				mov rdi, arg
			.endif
			.if i==2
				mov rsi, arg
			.endif
			.if i==3
				mov rdx, arg
			.endif
			.if i==4
				mov rcx, arg
			.endif
			.if i==5
				mov r8, arg
			.endif
			.if i==6
				mov r9, arg
			.endif
			.if i>6
				j = 0
				.irp param \args
					j = j + 1
					.if j==(n-i+7)
						push param
					.endif
				.endr
			.endif
		.endif
	.endr
    mov    rax, 0
	call \fun
	i = 8*(n-6)
	.if i>0
		add rsp, i
	.endif	
.endm


/*****************************************************************
						FUNCTION MACROS
*****************************************************************/

.macro .add. a:req args:vararg
	.if \a != rax
		mov rax, \a
	.endif	
	.irp arg,\args
		.ifnb arg,
    		add rax, arg
		.endif
	.endr
.endm

/*****************************************************************
							RUNTIME
*****************************************************************/

.data
.balign, 8
stop: .quad .
.lcomm memory, 1024
top: .quad .
brk: .quad offset(memory)

