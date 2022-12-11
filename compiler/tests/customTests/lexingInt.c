#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

int main() {
  for(int i=0xcccc; i<0xffff; i++){
    if(i==0xdddd)
      break;
  }
}
