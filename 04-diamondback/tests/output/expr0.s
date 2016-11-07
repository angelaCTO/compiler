section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 16
  mov eax, 4
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 6
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 4
  add eax, 6
  jo  near internal_error_overflow
  mov [ebp - 4], eax
  mov eax, 24
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 8
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 24
  sub eax, 8
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
  sar eax, 1
  imul eax, [ebp - 8]
  jo  near internal_error_overflow
  mov [ebp - 12], eax
  mov eax, 14
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 16
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 14
  add eax, 16
  jo  near internal_error_overflow
  mov [ebp - 16], eax
  mov eax, [ebp - 12]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 16]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 12]
  sar eax, 1
  imul eax, [ebp - 16]
  jo  near internal_error_overflow
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
