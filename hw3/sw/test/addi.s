.global _start
_start:
  addi   x1, x1, 0x10
  ld     x1, 0(x1)
  csrrwi x0, 0, 0

