#include <stdio.h>
#include <stdlib.h>
#define ZEROBIT       0x00000001
#define CONST_TRUE    0xFFFFFFFF
#define CONST_FALSE   0x7FFFFFFF

extern int  our_code_starts_here() asm("our_code_starts_here");
extern int  print(int val)         asm("print");
extern void error(int code, int v) asm("error");
extern int  print_val(int val);

/* Error Codes
 0 : non-number
 1 : non-boolean
 2 : overflow
 3 : non-tuple
 4 : index-too-small
 5 : index-too-small
 */


 void error(int code, int v){
   if (code == 0) {
     fprintf(stderr, "Dynamic Error: expected a number but got %#010x\n", v);
   }
   else if (code == 1) {
     fprintf(stderr, "Dynamic Error: expected a boolean but got %#010x\n", v);
   }
   else if (code == 2) {
     fprintf(stderr, "Dynamic Error: expected a closure but got %#010x\n", v);
   }
   else if (code == 3) {
     fprintf(stderr, "Dynamic Error: arithmetic overflow.");
   }
   else if (code == 4){
     fprintf(stderr, "Dynamic Error: expected a tuple but got %#010x\n", v);
   }
   else if (code == 5){
     fprintf(stderr, "Dynamic Error: tuple index too small.");
   }
   else if (code == 6){
     fprintf(stderr, "Dynamic Error: tuple index too large.");
   }
   else if (code == 7){
     fprintf(stderr, "Dynamic Error: function call arity mismatch");
   }
   exit(1);
 }

int is_number(int v){
  return ((v & 1) == 0);
}

int is_boolean(int v){
  return ((v & CONST_FALSE) == CONST_FALSE);
}

int is_tuple(int v){
  return ((v & 7) == 1);
}

int is_closure(int v){
  return ((v & 7) == 5);
}

void print_number(int val){
  printf("%d", val >> 1);
}

void print_boolean(int val){
  if (val == CONST_TRUE)
    printf("true");
  else // if (val == CONST_FALSE)
    printf("false");
}

void print_tuple(int val){
  int *base = (int *) (val - 1);
  printf("(");
  print_val(base[0]);
  printf(", ");
  print_val(base[1]);
  printf(")");
}

int print_val(int val) {
  if (is_number(val))
    print_number(val);
  else if (is_boolean(val))
    print_boolean(val);
  else if (is_tuple(val))
    print_tuple(val);
  else if (is_closure(val))
    printf("<function>");
  else
    printf("Unknown value: %#010x", val);
  return val;
}

int print(int val){
  print_val(val);
  printf("\n");
  return val;
}

int main(int argc, char** argv) {
  int* HEAP = calloc(100000, sizeof (int));
  int result = our_code_starts_here(HEAP);
  print(result);
  return 0;
}
