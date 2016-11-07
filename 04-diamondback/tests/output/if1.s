section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 4
  push DWORD 10
  call print
  add esp, 4
  mov [ebp - 4], eax
  mov eax, [ebp - 4]
  and eax, 1
  cmp eax, 1
  jne near internal_error_non_boolean
  mov eax, [ebp - 4]
  cmp eax, 0x7FFFFFFF
  jne near label_6_true
  mov eax, 4
  jmp near label_6_done
label_6_true: 
  mov eax, 8
label_6_done: 
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
