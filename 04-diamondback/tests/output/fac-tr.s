section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 0
  push DWORD 10
  push DWORD 2
  call label_def_facTR_start
  add esp, 8
  mov esp, ebp
  pop  ebp
  ret
label_def_facTR_start: 
  push ebp
  mov ebp, esp
  sub esp, 12
  mov eax, [ebp + 12]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 2
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp + 12]
  cmp eax, 2
  jl  near label_3_true
  mov eax, 0x7FFFFFFF
  jmp near label_3_done
label_3_true: 
  mov eax, 0xFFFFFFFF
label_3_done: 
  mov [ebp - 4], eax
  mov eax, [ebp - 4]
  and eax, 1
  cmp eax, 1
  jne near internal_error_non_boolean
  mov eax, [ebp - 4]
  cmp eax, 0x7FFFFFFF
  jne near label_19_true
  mov eax, [ebp + 8]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp + 12]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp + 8]
  sar eax, 1
  imul eax, [ebp + 12]
  jo  near internal_error_overflow
  mov [ebp - 8], eax
  mov eax, [ebp + 12]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 2
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp + 12]
  sub eax, 2
  jo  near internal_error_overflow
  mov [ebp - 12], eax
  jmp near label_19_done
label_19_true: 
  mov eax, [ebp + 8]
label_19_done: 
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
