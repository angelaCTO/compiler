section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 0
  mov eax, 2
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 0xFFFFFFFF
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 2
  sar eax, 1
  imul eax, 0xFFFFFFFF
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
