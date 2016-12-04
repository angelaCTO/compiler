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
  sub esp, 28
  and esp, 0xFFFFFFF0
  mov eax, esi
  mov DWORD [eax + 0], 4
  add esi, 16
  mov ebx, 20
  mov DWORD [eax + 4], ebx
  mov ebx, 40
  mov DWORD [eax + 4], ebx
  or  eax, 0x1
  mov [ebp - 4], eax
  mov eax, esi
  mov DWORD [eax + 0], 4
  add esi, 16
  mov ebx, [ebp - 4]
  mov DWORD [eax + 4], ebx
  mov ebx, 60
  mov DWORD [eax + 4], ebx
  or  eax, 0x1
  mov [ebp - 8], eax
  sub esp, 12
  push DWORD [ebp - 8]
  call print
  add esp, 16
  mov [ebp - 4], eax
  mov eax, [ebp - 4]
  and eax, 0xFFFFFFF8
  mov ebx, 1
  shr ebx, 1
  add ebx, 1
  mov eax, [eax + ebx * 4]
  mov [ebp - 8], eax
  mov eax, [ebp - 4]
  and eax, 0xFFFFFFF8
  mov ebx, 0
  shr ebx, 1
  add ebx, 1
  mov eax, [eax + ebx * 4]
  mov [ebp - 12], eax
  mov eax, [ebp - 12]
  and eax, 0xFFFFFFF8
  mov ebx, 1
  shr ebx, 1
  add ebx, 1
  mov eax, [eax + ebx * 4]
  mov [ebp - 16], eax
  mov eax, [ebp - 4]
  and eax, 0xFFFFFFF8
  mov ebx, 0
  shr ebx, 1
  add ebx, 1
  mov eax, [eax + ebx * 4]
  mov [ebp - 20], eax
  mov eax, [ebp - 20]
  and eax, 0xFFFFFFF8
  mov ebx, 0
  shr ebx, 1
  add ebx, 1
  mov eax, [eax + ebx * 4]
  mov [ebp - 24], eax
  mov eax, esi
  mov DWORD [eax + 0], 4
  add esi, 16
  mov ebx, [ebp - 16]
  mov DWORD [eax + 4], ebx
  mov ebx, [ebp - 24]
  mov DWORD [eax + 4], ebx
  or  eax, 0x1
  mov [ebp - 28], eax
  mov eax, esi
  mov DWORD [eax + 0], 4
  add esi, 16
  mov ebx, [ebp - 8]
  mov DWORD [eax + 4], ebx
  mov ebx, [ebp - 28]
  mov DWORD [eax + 4], ebx
  or  eax, 0x1
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
