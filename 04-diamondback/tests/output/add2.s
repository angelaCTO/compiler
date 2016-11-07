section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 0
  push DWORD 14
  push DWORD 24
  call label_def_add2_start
  add esp, 8
  mov esp, ebp
  pop  ebp
  ret
label_def_add2_start: 
  push ebp
  mov ebp, esp
  sub esp, 0
  mov eax, [ebp + 8]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp + 12]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp + 8]
  add eax, [ebp + 12]
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
