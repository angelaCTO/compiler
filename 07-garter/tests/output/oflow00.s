section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  mov esi, [esp + 4]
  add esi, 8
  and esi, 0xFFFFFFF8
  push ebp
  mov ebp, esp
  sub esp, 32
  and esp, 0xFFFFFFF0
  mov eax, 4
  mov [ebp - 4], eax
  mov eax, [ebp - 4]
  mov ebx, [ebp - 4]
  sar eax, 1
  imul eax, ebx
  jo  near internal_error_overflow
  mov [ebp - 8], eax
  mov eax, [ebp - 8]
  mov ebx, [ebp - 8]
  sar eax, 1
  imul eax, ebx
  jo  near internal_error_overflow
  mov [ebp - 12], eax
  mov eax, [ebp - 12]
  mov ebx, [ebp - 12]
  sar eax, 1
  imul eax, ebx
  jo  near internal_error_overflow
  mov [ebp - 16], eax
  mov eax, [ebp - 16]
  mov ebx, [ebp - 16]
  sar eax, 1
  imul eax, ebx
  jo  near internal_error_overflow
  mov [ebp - 20], eax
  mov eax, [ebp - 20]
  mov ebx, [ebp - 20]
  sar eax, 1
  imul eax, ebx
  jo  near internal_error_overflow
  mov [ebp - 24], eax
  mov eax, [ebp - 24]
  mov ebx, [ebp - 24]
  sar eax, 1
  imul eax, ebx
  jo  near internal_error_overflow
  mov [ebp - 28], eax
  mov eax, [ebp - 28]
  mov ebx, [ebp - 28]
  sar eax, 1
  imul eax, ebx
  jo  near internal_error_overflow
  mov [ebp - 32], eax
  mov eax, [ebp - 32]
  mov esp, ebp
  pop  ebp
  ret
internal_error_non_number: 
  sub esp, 8
  push eax
  push 0
  call error
internal_error_non_boolean: 
  sub esp, 8
  push eax
  push 1
  call error
internal_error_non_tuple: 
  sub esp, 8
  push eax
  push 4
  call error
internal_error_non_closure: 
  sub esp, 8
  push eax
  push 2
  call error
internal_error_overflow: 
  sub esp, 8
  push eax
  push 3
  call error
internal_error_index_too_low: 
  sub esp, 8
  push eax
  push 5
  call error
internal_error_index_too_high: 
  sub esp, 8
  push eax
  push 6
  call error
internal_error_wrong_arity: 
  sub esp, 8
  push eax
  push 7
  call error
