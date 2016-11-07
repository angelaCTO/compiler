section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 8
  mov eax, 0xFFFFFFFF
  mov [ebp - 4], eax
  push DWORD 0x7FFFFFFF
  call print
  add esp, 4
  mov [ebp - 8], eax
  mov eax, [ebp - 4]
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
