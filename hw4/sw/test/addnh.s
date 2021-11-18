.global _start
_start:
  addi   x1, x0, 0x10
  addi   x2, x0, 0x20
  nop
  nop
  nop
  add   x3, x1, x2
  nop
  nop
  nop
  ld     x1, 0(x3)
  csrrwi x0, 0, 0
