.global _start
_start:
  addi   x1, x0, 0x204
  addi   x2, x0, 0x204
  nop
  nop
  nop
  ld     x1, 0(x1)
  ld     x2, 0(x2)
  nop
  nop
  nop
  nop
  beq    x1, x2, target
  nop
  nop
  nop
  nop
  csrrwi x0, 0, 0
  nop
  nop
  sd     x1, 0(x0)
target:
  nop
  nop
  sd     x1, 0(x0)
  beq    x1, x0, target2
  nop
  nop
  nop
  nop
  csrrwi x0, 0, 0
target2:
  nop
  nop
  nop
  nop
  sd     x0, 0(x0)
  nop
  nop
  csrrwi x0, 0, 0

