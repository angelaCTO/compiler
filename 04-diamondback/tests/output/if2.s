section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 0
  mov eax, 0
  and eax, 1
  cmp eax, 1
  jne near internal_error_non_boolean
  mov eax, 0
  cmp eax, 0x7FFFFFFF
  jne near label_4_true
  mov eax, 4
  jmp near label_4_done
label_4_true: 
  mov eax, 8
label_4_done: 
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
