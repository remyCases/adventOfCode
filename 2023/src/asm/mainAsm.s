.global _start
.intel_syntax noprefix

.section .text

_print_fn:
	push ebp
	mov ebp, esp
	mov eax, 4
	mov ebx, 1
	mov ecx, [ebp + 8]	# 1st argument is the char to print
	mov edx, 1
	int 0x80
	push '\n'		# print new line with system call
	mov eax, 4
	mov ebx, 1
	mov ecx, esp
	mov edx, 1
	int 0x80
	add esp, 4		# pop '\n'
	pop ebp
	ret 4

_start:
	xor ecx, ecx
	mov val, ecx
	lea esi, [example]
_startloop:
	mov dl, [esi]		# load 1 byte from esi
	test dl, dl		# exit if null
	jz _exit
	push ecx		# save ecx
	cmp edx, '9'
	jg _endloop
	cmp edx, '1'
	jl _endloop
_compute_first_digit:
	mov stored, dl 		# store last encountered digit
	mov eax, val
	test eax, eax
	jnz _endloop
	mov eax, 1
	mov val, al
_print:
	mov eax, 4
	mov ebx, 1
	mov ecx, esi
	mov edx, 1
	int 0x80
_newline:			# print new line with system call
	push '\n'
	mov eax, 4
	mov ebx, 1
	mov ecx, esp
	mov edx, 1
	int 0x80
	add esp, 4		# pop '\n'
_endloop:
	pop ecx			# restore ecx
	inc ecx
	inc esi
	cmp ecx, 100		# exit if more than 100 chars
	jne _startloop
_exit:
	push [stored]
	call _print_fn	 
	# Exit cleanly
	mov eax, 1		# SYS_EXIT
	xor ebx, ebx		# exit value
	int 0x80		# sys call

.section .data
	example:	.ascii "a1eer895fv"
	str_result:	.ascii "CALIBRATION VALUE"
	stored: 	.ascii "r"
	val: 		.word 0
