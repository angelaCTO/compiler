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
  sub esp, 12
  and esp, 0xFFFFFFF0
  mov eax, 20
  mov [ebp - 4], eax
  mov eax, [ebp - 4]
  mov ebx, 2
  add eax, ebx
  jo  near internal_error_overflow
  mov [ebp - 8], eax
  mov eax, [ebp - 4]
  mov ebx, [ebp - 8]
  cmp eax, ebx
  je  near label_6_true
  mov eax, 0x7FFFFFFF
  jmp near label_6_done
label_6_true: 
  mov eax, 0xFFFFFFFF
label_6_done: 
  mov [ebp - 12], eax
  mov eax, [ebp - 12]
  cmp eax, 0x7FFFFFFF
  je  near label_10_true
  mov eax, 200
  jmp near label_10_done
label_10_true: 
  mov eax, 88
label_10_done: 
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
