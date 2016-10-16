#include <stdio.h>
#include <stdlib.h>

extern int our_code_starts_here() asm("our_code_starts_here");

int print(int val) {
  if ( (val ^ 0xFFFFFFFF) == 0 ){
    printf("true\n");
  }else if ( (val ^ 0x7FFFFFFF) == 0){
    printf("false\n");
  } else {
    printf("%d\n", val>>1);
  }
  return val;
}

int main(int argc, char** argv) {
  int result = our_code_starts_here();
  print(result);
  return 0;
}
