.global _start
_start:
  addi   x1, x0, 0x204
  addi   x2, x0, 0x204
  ld     x1, 0(x1)
  ld     x2, 0(x2)
  add    x3, x1, x2
  beq    x1, x2, target
  csrrwi x0, 0, 0
  sd     x1, 0(x0)
target:
  sd     x1, 0(x0)
  beq    x1, x0, target2
  csrrwi x0, 0, 0
target2:
  sd     x0, 0(x0)
  csrrwi x0, 0, 0

