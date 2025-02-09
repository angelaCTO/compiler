#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern int our_code_starts_here() asm("our_code_starts_here");
extern int print(int val) asm("print");

void error (int err){
  if(err == 0)
    fprintf(stderr, "Error: expected a number\n");
  if(err == 1)
    fprintf(stderr, "Error: expected a boolean\n");
  if(err == 2)
    fprintf(stderr, "Error: arithmetic overflow\n");
  exit( 1 );
}

int print(int val) {
  if(val & 0x00000001 ^ 0x00000001) {
    printf("%d\n", val >> 1);
  }
  else if(val == 0xFFFFFFFF) {
    printf("true\n");
  }
  else if(val == 0x7FFFFFFF) {
    printf("false\n");
  }
  else {
    printf("Unknown value: %#010x\n", val);
  }
  return val;
}

/*

   Copy over any error-detection functions here

*/


// main should remain unchanged
int main(int argc, char** argv) {
  int result = our_code_starts_here();
  print(result);
  return 0;
}
