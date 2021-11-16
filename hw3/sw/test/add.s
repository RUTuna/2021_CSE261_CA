.global _start
_start:
  addi   x1, x0, 0x210
  addi   x2, x0, 0x120
  add    x3, x1, x2
  ld     x1, 0(x3)
  csrrwi x0, 0, 0
