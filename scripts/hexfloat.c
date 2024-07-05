/*
 * hexfloat.c
 *
 * This is a simple program that helps interpret the hex representation of a
 * floating point number.
 *
 */

#include "stdio.h"

int main(int argc, char *argv[]) {
  if (argc != 2) {
    printf("Usage: %s <hexfloat>\n", argv[0]);
    return 1;
  }

  float f;
  sscanf(argv[1], "%f", &f);
  printf("%f 0x%08x\n", f, *(int *)&f);
  return 0;
}