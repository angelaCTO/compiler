section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 20
  mov eax, 0xFFFFFFFF
  and eax, 1
  cmp eax, 1
  jne near internal_error_non_boolean
  mov eax, 0xFFFFFFFF
  cmp eax, 0x7FFFFFFF
  jne near label_29_true
  mov eax, 24
  jmp near label_29_done
label_29_true: 
  mov eax, 4
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 4
  add eax, 2
  jo  near internal_error_overflow
  mov [ebp - 4], eax
  mov eax, 2
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 4]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 2
  add eax, [ebp - 4]
  jo  near internal_error_overflow
  mov [ebp - 8], eax
  mov eax, 6
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 8
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 6
  add eax, 8
  jo  near internal_error_overflow
  mov [ebp - 12], eax
  mov eax, [ebp - 12]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 12]
  sub eax, 2
  jo  near internal_error_overflow
  mov [ebp - 16], eax
  mov eax, [ebp - 8]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 16]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 8]
  add eax, [ebp - 16]
  jo  near internal_error_overflow
  mov [ebp - 20], eax
  mov eax, [ebp - 20]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 20
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 20]
  add eax, 20
  jo  near internal_error_overflow
label_29_done: 
  mov esp, ebp
  pop  ebp
  ret
internal_error_non_number: 
  push 0
  call error
  ret
internal_error_non_boolean: 
  push 1
  call error
  ret
internal_error_overflow: 
  push 2
  call error
  ret
