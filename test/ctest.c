#include <stdint.h>
#include <stdio.h>

extern uint32_t foo(uint32_t a);
extern uint32_t ayo(uint32_t a, uint32_t b, uint32_t c, uint32_t d, uint32_t e,
                    uint32_t f, uint32_t g, uint32_t h);
extern uint32_t max(uint32_t a, uint32_t b);

// This is called from `foo` in our own compiled code.
uint32_t bar(uint32_t x) { return max(x, 10); }

int main() {
  uint32_t f = foo((uint32_t)425);
  printf("foo(425) = %d\n", f);
  uint32_t f2 = foo((uint32_t)9);
  printf("foo(9) = %d\n", f2);
  return 0;
}

/*
 * extern fn bar(x: usize): usize;
 * extern fn foo(x: usize): usize {
 *     return x - bar(0);
 * }
 */
