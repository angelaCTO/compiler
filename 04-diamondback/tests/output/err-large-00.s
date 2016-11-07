section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 0
  push DWORD 10
  call label_def_incr_start
  add esp, 4
  mov esp, ebp
  pop  ebp
  ret
label_def_incr_start: 
  push ebp
  mov ebp, esp
  sub esp, 0
  mov eax, [ebp + 8]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 81474836520
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp + 8]
  add eax, 81474836520
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
