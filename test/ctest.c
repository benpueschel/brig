#include <stdint.h>
#include <stdio.h>

struct Vec2 {
  uint32_t x;
  uint32_t y;
};
extern struct Vec2 vec2(uint32_t x, uint32_t y);

extern uint32_t max(uint32_t a, uint32_t b);
extern uint32_t maxten(uint32_t a);

extern void print_bool(char x) { printf("%u\n", x); }
extern void print_int(uint32_t x) { printf("%u\n", x); }

int main() {
  uint32_t f = maxten((uint32_t)425);
  printf("maxten(425) = %u\n", f);
  uint32_t f2 = max((uint32_t)9, (uint32_t)10);
  printf("max(9, 10) = %u\n", f2);
  struct Vec2 v = vec2(1, 2);
  printf("vec2(1, 2) = (%u, %u)\n", v.x, v.y);

  return 0;
}

/*
 * extern fn bar(x: usize): usize;
 * extern fn foo(x: usize): usize {
 *     return x - bar(0);
 * }
 */
