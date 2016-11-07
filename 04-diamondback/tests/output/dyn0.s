section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 12
  mov eax, 0xFFFFFFFF
  mov [ebp - 4], eax
  mov eax, [ebp - 4]
  and eax, 0x1
  cmp eax, 0
  je  near label_3_true
  mov eax, 0x7FFFFFFF
  jmp near label_3_done
label_3_true: 
  mov eax, 0xFFFFFFFF
label_3_done: 
  mov [ebp - 8], eax
  mov eax, [ebp - 8]
  and eax, 1
  cmp eax, 1
  jne near internal_error_non_boolean
  mov eax, [ebp - 8]
  cmp eax, 0x7FFFFFFF
  jne near label_13_true
  mov eax, 24
  jmp near label_13_done
label_13_true: 
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
  mov [ebp - 12], eax
  push DWORD [ebp - 12]
  call print
  add esp, 4
label_13_done: 
  mov [ebp - 8], eax
  mov eax, [ebp - 8]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 20
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 8]
  sar eax, 1
  imul eax, 20
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
