#include <stdint.h>
#include <stdio.h>

extern uint32_t foo(uint32_t a);
extern uint32_t ayo(uint32_t a, uint32_t b, uint32_t c, uint32_t d, uint32_t e,
                    uint32_t f, uint32_t g, uint32_t h);

uint32_t bar(uint32_t x) { return ayo(x, 1, 2, 3, 4, 5, 6, 7); }

int main() {
  uint32_t f = foo((uint32_t)425); // This will return 420 :)
  printf("foo(425) = %d\n", f);
  return 0;
}

/*
 * extern fn bar(x: usize): usize;
 * extern fn foo(x: usize): usize {
 *     return x - bar(0);
 * }
 */
