.global _start
_start:
  auipc  x1, %pcrel_hi(data)
  addi   x1, x1, %pcrel_lo(_start)
  ld     x2, 0(x1)
  csrrwi x0, 0, 0
  nop


