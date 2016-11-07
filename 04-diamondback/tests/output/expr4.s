section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 44
  mov eax, 14
  mov [ebp - 4], eax
  mov eax, 6
  mov [ebp - 8], eax
  mov eax, 8
  mov [ebp - 12], eax
  mov eax, [ebp - 8]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 12]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 8]
  add eax, [ebp - 12]
  jo  near internal_error_overflow
  mov [ebp - 16], eax
  mov eax, [ebp - 4]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 16]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 4]
  sub eax, [ebp - 16]
  jo  near internal_error_overflow
  mov [ebp - 20], eax
  mov eax, [ebp - 20]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 0
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 20]
  cmp eax, 0
  je  near label_12_true
  mov eax, 0x7FFFFFFF
  jmp near label_12_done
label_12_true: 
  mov eax, 0xFFFFFFFF
label_12_done: 
  mov [ebp - 24], eax
  mov eax, [ebp - 24]
  and eax, 1
  cmp eax, 1
  jne near internal_error_non_boolean
  mov eax, [ebp - 24]
  cmp eax, 0x7FFFFFFF
  jne near label_61_true
  mov eax, 4
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 4
  add eax, 2
  jo  near internal_error_overflow
  mov [ebp - 28], eax
  mov eax, 2
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 28]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 2
  add eax, [ebp - 28]
  jo  near internal_error_overflow
  mov [ebp - 32], eax
  mov eax, 6
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 8
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 6
  add eax, 8
  jo  near internal_error_overflow
  mov [ebp - 36], eax
  mov eax, [ebp - 36]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 36]
  sub eax, 2
  jo  near internal_error_overflow
  mov [ebp - 40], eax
  mov eax, [ebp - 32]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 40]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 32]
  add eax, [ebp - 40]
  jo  near internal_error_overflow
  mov [ebp - 44], eax
  mov eax, [ebp - 44]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 20
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 44]
  add eax, 20
  jo  near internal_error_overflow
  jmp near label_61_done
label_61_true: 
  mov eax, 4
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 4
  add eax, 2
  jo  near internal_error_overflow
  mov [ebp - 28], eax
  mov eax, 2
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 28]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 2
  sub eax, [ebp - 28]
  jo  near internal_error_overflow
  mov [ebp - 32], eax
  mov eax, 6
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 6
  sub eax, 2
  jo  near internal_error_overflow
  mov [ebp - 36], eax
  mov eax, [ebp - 32]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 36]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 32]
  sar eax, 1
  imul eax, [ebp - 36]
  jo  near internal_error_overflow
  mov [ebp - 40], eax
  mov eax, [ebp - 40]
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, 4
  and eax, 1
  cmp eax, 0
  jne near internal_error_non_number
  mov eax, [ebp - 40]
  sar eax, 1
  imul eax, 4
  jo  near internal_error_overflow
label_61_done: 
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
