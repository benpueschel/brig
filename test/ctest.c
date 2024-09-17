#include <stdint.h>
#include <stdio.h>

extern uint32_t max(uint32_t a, uint32_t b);
extern uint32_t maxten(uint32_t a);

extern void print_bool(char x) { printf("%u\n", x); }
extern void print_int(uint32_t x) { printf("%u\n", x); }

int main() {
  uint32_t f = maxten((uint32_t)425);
  printf("maxten(425) = %u\n", f);
  uint32_t f2 = max((uint32_t)9, (uint32_t)10);
  printf("max(9, 10) = %u\n", f2);
  return 0;
}

/*
 * extern fn bar(x: usize): usize;
 * extern fn foo(x: usize): usize {
 *     return x - bar(0);
 * }
 */
