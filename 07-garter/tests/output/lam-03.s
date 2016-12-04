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
  sub esp, 20
  and esp, 0xFFFFFFF0
  jmp near label_11_lam_end
label_11_lam_start: 
  push ebp
  mov ebp, esp
  sub esp, 4
  and esp, 0xFFFFFFF0
  mov eax, [ebp + 12]
  mov ebx, 2
  add eax, ebx
  jo  near internal_error_overflow
  mov [ebp - 4], eax
  jmp near label_7_lam_end
label_7_lam_start: 
  push ebp
  mov ebp, esp
  sub esp, 4
  and esp, 0xFFFFFFF0
  mov eax, [ebp + 8]
  and eax, 0xFFFFFFF8
  mov ebx, 4
  shr ebx, 1
  mov eax, [eax + ebx * 4]
  mov [ebp - 4], eax
  mov eax, [ebp + 12]
  mov ebx, [ebp - 4]
  add eax, ebx
  jo  near internal_error_overflow
  mov esp, ebp
  pop  ebp
  ret
label_7_lam_end: 
  mov eax, esi
  mov DWORD [eax + 0], 2
  add esi, 24
  mov ebx, label_7_lam_start
  mov DWORD [eax + 4], ebx
  mov ebx, [ebp - 4]
  mov DWORD [eax + 8], ebx
  or  eax, 0x5
  mov esp, ebp
  pop  ebp
  ret
label_11_lam_end: 
  mov eax, esi
  mov DWORD [eax + 0], 2
  add esi, 16
  mov ebx, label_11_lam_start
  mov DWORD [eax + 4], ebx
  or  eax, 0x5
  mov [ebp - 4], eax
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
  push DWORD 20
  push DWORD [ebp - 4]
  call eax
  add esp, 16
  mov [ebp - 12], eax
  mov eax, [ebp - 8]
  mov ebx, eax
  and ebx, 0x7
  cmp e