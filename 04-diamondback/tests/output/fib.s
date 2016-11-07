section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 24
  push DWORD 0
  call label_def_fib_start
  add esp, 4
  mov [ebp - 4], eax
  push DWORD [ebp - 4]
  call print
  add esp, 4
  mov [ebp - 4], eax
  push DWORD 2
  call label_def_fib_start
  add esp, 4
  mov [ebp - 8], eax
  push DWORD [ebp - 8]
  call print
  add esp, 4
  mov [ebp - 8], eax
  push DWORD 4
  call label_def_fib_start
  add esp, 4
  mov [ebp - 12], eax
  push DWORD [ebp - 12]
  call print
  add esp, 4
  mov [ebp - 12], eax
  push DWORD 6
  call label_def_fib_start
  add esp, 4
  mov [ebp - 16], eax
  push DWORD [ebp - 16]
  call print
  add esp, 4
  mov [ebp - 16], eax
  push DWORD 8
  call label_def_fib_start
  add esp, 4
  mov [ebp - 20], eax
  push DWORD [ebp - 20]
  call print
  add esp, 4
  mov [ebp - 20], eax
  push DWORD 10
  call label_def_fib_start
  add esp, 4
  mov [ebp - 24], eax
  push DWORD [ebp - 24]
  call print
  add esp, 4
  mov [ebp - 24], eax
  mov eax, 0
  mov esp, ebp
  pop  ebp
  ret
label_def_fib_start: 
  push ebp
  mov ebp, esp
  sub esp, 20
  mov eax, [ebp + 8]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 4
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp + 8]
  cmp eax, 4
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
  jne near label_27_true
  mov eax, [ebp + 8]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 2
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp + 8]
  sub eax, 2
  jo  near internal_error_overflow
  mov [ebp - 8], eax
  push DWORD [ebp - 8]
  call label_def_fib_start
  add esp, 4
  mov [ebp - 12], eax
  mov eax, [ebp + 8]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 4
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp + 8]
  sub eax, 4
  jo  near internal_error_overflow
  mov [ebp - 16], eax
  push DWORD [ebp - 16]
  call label_def_fib_start
  add esp, 4
  mov [ebp - 20], eax
  mov eax, [ebp - 12]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 20]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 12]
  add eax, [ebp - 20]
  jo  near internal_error_overflow
  jmp near label_27_done
label_27_true: 
  mov eax, 2
label_27_done: 
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
