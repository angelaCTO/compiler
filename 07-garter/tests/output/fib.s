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
  jmp near label_33_lam_end
label_33_lam_start: 
  push ebp
  mov ebp, esp
  sub esp, 20
  and esp, 0xFFFFFFF0
  mov eax, [ebp + 12]
  mov ebx, 4
  cmp eax, ebx
  jl  near label_2_true
  mov eax, 0x7FFFFFFF
  jmp near label_2_done
label_2_true: 
  mov eax, 0xFFFFFFFF
label_2_done: 
  mov [ebp - 4], eax
  mov eax, [ebp - 4]
  cmp eax, 0x7FFFFFFF
  je  near label_28_true
  mov eax, 2
  jmp near label_28_done
label_28_true: 
  mov eax, [ebp + 12]
  mov ebx, 2
  sub eax, ebx
  jo  near internal_error_overflow
  mov [ebp - 8], eax
  mov eax, [ebp + 8]
  mov ebx, eax
  and ebx, 0x7
  cmp ebx, 0x5
  jne near internal_error_non_closure
  mov eax, [ebp + 8]
  and eax, 0xFFFFFFF8
  mov ebx, [eax + 0]
  cmp ebx, 2
  jne near internal_error_wrong_arity
  mov eax, [ebp + 8]
  and eax, 0xFFFFFFF8
  mov ebx, 2
  shr ebx, 1
  mov eax, [eax + ebx * 4]
  sub esp, 8
  push DWORD [ebp - 8]
  push DWORD [ebp + 8]
  call eax
  add esp, 16
  mov [ebp - 12], eax
  mov eax, [ebp + 12]
  mov ebx, 4
  sub eax, ebx
  jo  near internal_error_overflow
  mov [ebp - 16], eax
  mov eax, [ebp + 8]
  mov ebx, eax
  and ebx, 0x7
  cmp ebx, 0x5
  jne near internal_error_non_closure
  mov eax, [ebp + 8]
  and eax, 0xFFFFFFF8
  mov ebx, [eax + 0]
  cmp ebx, 2
  jne near internal_error_wrong_arity
  mov eax, [ebp + 8]
  and eax, 0xFFFFFFF8
  mov ebx, 2
  shr ebx, 1
  mov eax, [eax + ebx * 4]
  sub esp, 8
  push DWORD [ebp - 16]
  push DWORD [ebp + 8]
  call eax
  add esp, 16
  mov [ebp - 20], eax
  mov eax, [ebp - 12]
  mov ebx, [ebp - 20]
  add eax, ebx
  jo  near internal_error_overflow
label_28_done: 
  mov esp, ebp
  pop  ebp
  ret
label_33_lam_end: 
  mov eax, esi
  mov DWORD [eax + 0], 2
  add esi, 16
  mov ebx, label_33_lam_start
  mov DWORD [eax + 4], ebx
  or  eax, 0x5
  mov [ebp - 4], eax
  mov eax, 24
  mov [ebp - 8], eax
  mov eax, [ebp - 4]
  mov ebx, eax
  and ebx, 0x7
  cmp ebx, 0x5
  jne near internal_error_non_closure
  mov eax, [ebp - 4]
  and eax, 0xFFFFFFF8
  mov ebx, [eax + 0]
  cmp ebx, 2
  jne near internal_error_wrong_arity
  mov eax, [ebp - 4]
  and eax, 0xFFFFFFF8
  mov ebx, 2
  shr ebx, 1
  mov eax, [eax + ebx * 4]
  sub esp, 8
  push DWORD 0
  push DWORD [ebp - 4]
  call eax
  add esp, 16
  mov [ebp - 12], eax
  sub esp, 12
  push DWORD [ebp - 12]
  call print
  add esp, 16
  mov [ebp - 12], eax
  mov eax, [ebp - 4]
  mov ebx, eax
  and ebx, 0x7
  cmp ebx, 0x5
  jne near internal_error_non_closure
  mov eax, [ebp - 4]
  and eax, 0xFFFFFFF8
  mov ebx, [eax + 0]
  cmp ebx, 2
  jne near internal_error_wrong_arity
  mov eax, [ebp - 4]
  and eax, 0xFFFFFFF8
  mov ebx, 2
  shr ebx, 1
  mov eax, [eax + ebx * 4]
  sub esp, 8
  push DWORD 2
  push DWORD [ebp - 4]
  call eax
  add esp, 16
  mov [ebp - 16], eax
  sub esp, 12
  push DWORD [ebp - 16]
  call print
  add esp, 16
  mov [ebp - 16], eax
  mov eax, [ebp - 4]
  mov ebx, eax
  and ebx, 0x7
  cmp ebx, 0x5
  jne near internal_error_non_closure
  mov eax, [ebp - 4]
  and eax, 0xFFFFFFF8
  mov ebx, [eax + 0]
  cmp ebx, 2
  jne near internal_error_wrong_arity
  mov eax, [ebp - 4]
  and eax, 0xFFFFFFF8
  mov ebx, 2
  shr ebx, 1
  mov eax, [eax + ebx * 4]
  sub esp, 8
  push DWORD 4
  push DWORD [ebp - 4]
  call eax
  add esp, 16
  mov [ebp - 20], eax
  sub esp, 12
  push DWORD [ebp - 20]
  call print
  add esp, 16
  mov [ebp - 20], eax
  mov eax, [ebp - 4]
  mov ebx, eax
  and ebx, 0x7
  cmp ebx, 0x5
  jne near internal_error_non_closure
  mov eax, [ebp - 4]
  and eax, 0xFFFFFFF8
  mov ebx, [eax + 0]
  cmp ebx, 2
  jne near internal_error_wrong_arity
  mov eax, [ebp - 4]
  and eax, 0xFFFFFFF8
  mov ebx, 2
  shr ebx, 1
  mov eax, [eax + ebx * 4]
  sub esp, 8
  push DWORD 6
  push DWORD [ebp - 4]
  call eax
  add esp, 16
  mov [ebp - 24], eax
  sub esp, 12
  push DWORD [ebp - 24]
  call print
  add esp, 16
  mov [ebp - 24], eax
  mov eax, [ebp - 4]
  mov ebx, eax
  and ebx, 0x7
  cmp ebx, 0x5
  jne near internal_error_non_closure
  mov eax, [ebp - 4]
  and eax, 0xFFFFFFF8
  mov ebx, [eax + 0]
  cmp ebx, 2
  jne near internal_error_wrong_arity
  mov eax, [ebp - 4]
  and eax, 0xFFFFFFF8
  mov ebx, 2
  shr ebx, 1
  mov eax, [eax + ebx * 4]
  sub esp, 8
  push DWORD 8
  push DWORD [ebp - 4]
  call eax
  add esp, 16
  mov [ebp - 28], eax
  sub esp, 12
  push DWORD [ebp - 28]
  call print
  add esp, 16
  mov [ebp - 28], eax
  mov eax, [ebp - 4]
  mov ebx, eax
  and ebx, 0x7
  cmp ebx, 0x5
  jne near internal_error_non_closure
  mov eax, [ebp - 4]
  and eax, 0xFFFFFFF8
  mov ebx, [eax + 0]
  cmp ebx, 2
  jne near internal_error_wrong_arity
  mov eax, [ebp - 4]
  and eax, 0xFFFFFFF8
  mov ebx, 2
  shr ebx, 1
  mov eax, [eax + ebx * 4]
  sub esp, 8
  push DWORD 10
  push DWORD [ebp - 4]
  call eax
  add esp, 16
  mov [ebp - 32], eax
  sub esp, 12
  push DWORD [ebp - 32]
  call print
  add esp, 16
  mov [ebp - 32], eax
  mov eax, 0
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
