package Hw3

import chisel3._
import chisel3.util._
import chisel3.iotesters.PeekPokeTester
import firrtl_interpreter._
import scala.collection.mutable.ListBuffer
import scala.sys.process._
import scala.util.control._

object Hw3Tests {
  // (name, sourceName, result)
  // Test appear later assumes the test appear ealier succeeds.
  val testHome = "sw/test"
  val instructionTests = List(
  ("addi", "addi.s",
  """[0] pc: 0, insn: 1000093, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[1] pc: 4, insn: b083, write: 0, wdata: 0, addr: 16, read: 1, rdata: 0, halted: 0
[2] pc: 8, insn: 5073, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[3] pc: c, insn: 53, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 1""")
  ,("add", "add.s",
  """[0] pc: 0, insn: 21000093, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[1] pc: 4, insn: 12000113, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[2] pc: 8, insn: 2081b3, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[3] pc: c, insn: 1b083, write: 0, wdata: 0, addr: 816, read: 1, rdata: 330, halted: 0
[4] pc: 10, insn: 5073, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[5] pc: 14, insn: 53, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 1""")
  ,("ldsd", "ldsd.s",
  """[0] pc: 0, insn: 20400093, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[1] pc: 4, insn: 103423, write: 1, wdata: 204, addr: 8, read: 0, rdata: 0, halted: 0
[2] pc: 8, insn: 803103, write: 0, wdata: 0, addr: 8, read: 1, rdata: 204, halted: 0
[3] pc: c, insn: 80b183, write: 0, wdata: 0, addr: 524, read: 1, rdata: 20c, halted: 0
[4] pc: 10, insn: 5073, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[5] pc: 14, insn: 53, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 1""")
  ,("beq", "beq.s",
  """[0] pc: 0, insn: 20400093, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[1] pc: 4, insn: 20400113, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[2] pc: 8, insn: b083, write: 0, wdata: 0, addr: 516, read: 1, rdata: 204, halted: 0
[3] pc: c, insn: 13103, write: 0, wdata: 0, addr: 516, read: 1, rdata: 204, halted: 0
[4] pc: 10, insn: 2081b3, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[5] pc: 14, insn: 208663, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[6] pc: 20, insn: 103023, write: 1, wdata: 204, addr: 0, read: 0, rdata: 0, halted: 0
[7] pc: 24, insn: 8463, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[8] pc: 28, insn: 5073, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[9] pc: 2c, insn: 3023, write: 1, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 1""")
,("jal", "jal.s",
  """[0] pc: 0, insn: 1000513, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[1] pc: 4, insn: c000ef, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[2] pc: 10, insn: 2000513, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[3] pc: 14, insn: b103, write: 0, wdata: 0, addr: 8, read: 1, rdata: 0, halted: 0
[4] pc: 18, insn: 8067, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[5] pc: 8, insn: a03023, write: 1, wdata: 20, addr: 0, read: 0, rdata: 0, halted: 0
[6] pc: c, insn: 5073, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 0
[7] pc: 10, insn: 2000513, write: 0, wdata: 0, addr: 0, read: 0, rdata: 0, halted: 1""")
  )
}
