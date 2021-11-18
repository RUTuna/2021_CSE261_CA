.global _start
_start:
  addi   x1, x1, 0x10
  nop
  nop
  nop
  sd     x1, 0(x0)
  csrrwi x0, 0, 0

