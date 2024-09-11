#include <stdint.h>
#include <stdio.h>

extern uint64_t foo(uint64_t a);
extern uint64_t ayo(uint64_t a, uint64_t b, uint64_t c, uint64_t d, uint64_t e,
                    uint64_t f, uint64_t g, uint64_t h);

uint64_t bar(uint64_t x) { return ayo(x, 1, 2, 3, 4, 5, 6, 7); }

int main() {
  uint64_t f = foo((uint64_t)425); // This will return 420 :)
  printf("foo(425) = %lu\n", f);
  return 0;
}

/*
 * extern fn bar(x: usize): usize;
 * extern fn foo(x: usize): usize {
 *     return x - bar(0);
 * }
 */
