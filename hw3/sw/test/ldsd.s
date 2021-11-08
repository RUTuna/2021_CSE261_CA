.global _start
_start:
  addi   x1, x0, 0x204
  sd     x1, 8(x0)
  ld     x2, 8(x0)
  ld     x3, 8(x1)
  csrrwi x0, 0, 0