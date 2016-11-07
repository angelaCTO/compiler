section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 4
  mov eax, 2147483640
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 6
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 2147483640
  add eax, 6
  jo  near internal_error_overflow
  mov [ebp - 4], eax
  mov eax, [ebp - 4]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 10
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 4]
  add eax, 10
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
