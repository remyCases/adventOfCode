.intel_syntax noprefix
.global _main

# MACROS

.ifdef Linux
	.macro exit
		mov eax, 1		# SYS_EXIT
		xor ebx, ebx	# exit value
		int 0x80		# sys call
	.endm
.else
.ifdef Windows
	.extern _ExitProcess@4
	.macro exit
		push 0
		call _ExitProcess@4
	.endm
.else
.error "Not supported OS"
.endif
.endif

.extern _print_fn
.extern _day_one

.section .data

.section .text
_main:
	push ebp           # save old value of ebp
    mov ebp, esp       # new stack

	mov ecx, DWORD PTR [ebp+8]	# argc
    mov edx, [ebp + 12] 		# argv
	mov esi, 0x1	 			# counter
_args_loop:
	cmp esi, ecx
    jge _args_loop_end

	mov eax, [edx + esi * 4]	# argv[esi]
	
	inc esi
    jmp _args_loop
_args_loop_end:
	mov esp, ebp       # restore stack pointer
    pop ebp            # restore old value ebp
_exit:
	exit
