section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 8
  mov eax, 4
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 6
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 4
  sar eax, 1
  imul eax, 6
  jo  near internal_error_overflow
  mov [ebp - 4], eax
  mov eax, [ebp - 4]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 8
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 4]
  add eax, 8
  jo  near internal_error_overflow
  mov [ebp - 8], eax
  push DWORD [ebp - 8]
  call print
  add esp, 4
  mov [ebp - 4], eax
  mov eax, [ebp - 4]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 2
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 4]
  add eax, 2
  jo  near internal_error_overflow
  mov [ebp - 8], eax
  mov eax, [ebp - 4]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 8]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 4]
  cmp eax, [ebp - 8]
  jl  near label_18_true
  mov eax, 0x7FFFFFFF
  jmp near label_18_done
label_18_true: 
  mov eax, 0xFFFFFFFF
label_18_done: 
  mov [ebp - 8], eax
  mov eax, [ebp - 8]
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
