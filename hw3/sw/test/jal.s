.global _start
_start:
  addi   x10, x0, 0x10
  jal    x1, foo
  sd     x10, 0(x0)
  csrrwi x0, 0, 0
foo:
  addi   x10, x0, 0x20
  ld     x2, 0(x1)
  jalr   x0, 0(x1)
  csrrwi x0, 0, 0
  
  
