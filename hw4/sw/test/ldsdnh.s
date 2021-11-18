.global _start
_start:
  addi   x1, x0, 0x204
  nop
  nop
  nop
  sd     x1, 8(x0)
  nop
  ld     x2, 8(x0)
  ld     x3, 8(x1)
  nop
  nop
  sd     x1, 0(x2)
  csrrwi x0, 0, 0