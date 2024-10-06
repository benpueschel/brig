#include <stdint.h>
#include <stdio.h>

struct Vec2 {
  uint32_t x;
  uint32_t y;
};
struct Vec4 {
  struct Vec2 a;
  struct Vec2 b;
};
extern struct Vec2 vec2(uint32_t x, uint32_t y);
extern struct Vec4 vec4(struct Vec2 a, struct Vec2 b);

extern uint32_t max(uint32_t a, uint32_t b);
extern uint32_t maxten(uint32_t a);

extern void print_bool(char x) { printf("%u\n", x); }
extern void print_int(uint32_t x) { printf("%u\n", x); }

extern void debug_print(uint32_t x) { printf("debug_print: %u\n", x); }

int main() {
  uint32_t f = maxten((uint32_t)425);
  printf("maxten(425) = %u\n", f);
  uint32_t f2 = max((uint32_t)9, (uint32_t)10);
  printf("max(9, 10) = %u\n", f2);
  struct Vec2 v1 = vec2(1, 2);
  printf("vec2(1, 2) = (%u, %u)\n", v1.x, v1.y);
  struct Vec2 v2 = vec2(3, 4);
  printf("vec2(3, 4) = (%u, %u)\n", v2.x, v2.y);
  struct Vec4 v3 = vec4(v1, v2);
  printf("vec4(v1, v2) = ((%u, %u), (%u, %u))\n", v3.a.x, v3.a.y, v3.b.x,
         v3.b.y);

  return 0;
}

/*
 * extern fn bar(x: usize): usize;
 * extern fn foo(x: usize): usize {
 *     return x - bar(0);
 * }
 */
