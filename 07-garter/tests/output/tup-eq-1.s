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
  sub esp, 8
  and esp, 0xFFFFFFF0
  mov eax, esi
  mov DWORD [eax + 0], 4
  add esi, 16
  mov ebx, 8
  mov DWORD [eax + 4], ebx
  mov ebx, 10
  mov DWORD [eax + 4], ebx
  or  eax, 0x1
  mov [ebp - 4], eax
  mov eax, esi
  mov DWORD [eax + 0], 4
  add esi, 16
  mov ebx, 8
  mov DWORD [eax + 4], ebx
  mov ebx, 10
  mov DWORD [eax + 4], ebx
  or  eax, 0x1
  mov [ebp - 8], eax
  mov eax, [ebp - 4]
  mov ebx, [ebp - 8]
  cmp eax, ebx
  je  near label_8_true
  mov eax, 0x7FFFFFFF
  jmp near label_8_done
label_8_true: 
  mov eax, 0xFFFFFFFF
label_8_done: 
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
