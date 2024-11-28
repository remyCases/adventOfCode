.intel_syntax noprefix
.global _day_one

.section .data
	example:	.string "a1eer895fv"
	example_length = . - example
	str_result:	.string "CALIBRATION VALUE"
	new_line: .ascii "\n"

.section .text
_day_one:
    # start of function
	push ebp
	mov ebp, esp

	xor ecx, ecx
	lea esi, [example]
_startloop:
	mov dl, [esi]		# load 1 byte from esi
	test dl, dl			# exit if null
	jz _end
	push ecx			# save ecx
_print:
	push 1
	push esi
	call _print_fn
_newline:
	push 1
	push offset new_line
	call _print_fn
_endloop:
	pop ecx				# restore ecx
	inc ecx
	inc esi
	cmp ecx, 100		# exit if more than 100 chars
	jne _startloop
_end:
    # classic return
	mov esp, ebp
    pop ebp
	ret