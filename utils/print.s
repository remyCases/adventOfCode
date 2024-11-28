.intel_syntax noprefix

.extern _GetStdHandle@4
.extern _WriteConsoleA@20

.section .text
.global _print_fn
_print_fn:

.ifdef Linux
    # start of function
	push ebp
	mov ebp, esp

	mov eax, 4
	mov ebx, 1
    # parameters from stack
    mov ecx, [ebp + 8]          # ptr to the string
    mov edx, [ebp + 12]         # length of string

	int 0x80                    # sys call write

    # classic return
	mov esp, ebp
    pop ebp
	ret
.else
.ifdef Windows
    # start of function
    push ebp
	mov ebp, esp

    # parameters from stack
    mov ecx, [ebp + 8]          # ptr to the string
    mov edx, [ebp + 12]         # length of string

    push -11                    # STD_OUTPUT_HANDLE
    call _GetStdHandle@4

    sub esp, 20                 # write console needs 20bytes on the stack
    mov ebx, esp

    mov [ebx], eax              # getting the handle from _GetStdHandle
    mov [ebx + 4], ecx          # string
    mov [ebx + 8], edx          # len
    lea ecx, [ebx + 12]
    mov [ebx + 12], ecx         # store written written bytes
    mov dword ptr [ebx + 16], 0 # mandatory

    call _WriteConsoleA@20      # call write

    add esp, 20                 # clean stack

    # classic return
    mov esp, ebp
    pop ebp
    ret
.else
.error "Not supported OS"
.endif
.endif
