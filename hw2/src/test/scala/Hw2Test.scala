import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import chisel3._
import chisel3.util._
import Hw2._



class ALUTest extends FlatSpec with Matchers {

  "ALU" should "compute" in {
    chisel3.iotesters.Driver( () => new ALU()) { dut =>
      new PeekPokeTester(dut) {
        val r = new scala.util.Random()
        r.setSeed(System.currentTimeMillis())
        val inputs = List.range(0,20).map((i) => {
          val v1 = r.nextLong()
          val v2 = r.nextLong()
          val pair = (v1, v2)
          pair
        })
        val ctrls = List(0,1,2,6)
        val tests = (ctrls.map((c) => {
          
          inputs.map((p) => {
            val ctrl: Long = c
            val res: Long = (ctrl match {
              case 0 => p._1 & p._2
              case 1 => p._1 | p._2
              case 2 => (p._1 + p._2)
              case 6 => (p._1 - p._2)
              case _ => throw new Exception("Unknown ctrl")
            })
            Array(ctrl, p._1, p._2, res)
          })
        }).flatten ++ List(
          Array(2, 0x1, 0xfffffffffffffffeL, -1),
          Array(2, 0x1, 0xffffffffffffffffL, 0)
        )).map(e => {
          //Array(e(0).U(5.W), e(1).S(64.W), e(2).S(64.W), e(3).S(64.W))
          e
        }) 

        tests.foreach((t) => {
          poke(dut.io.ctrl, t(0))
          poke(dut.io.a, t(1))
          poke(dut.io.b, t(2))
          expect(dut.io.res, t(3).S(64.W)(63,0))
          
          
        })

      }
    } should be (true)
  }
}

class ImmGenTest extends FlatSpec with Matchers {
  "ImmGen" should "" in {
    chisel3.iotesters.Driver( () => new ImmGen()) { dut =>
      new PeekPokeTester(dut) {
        val tests = List(
          List("x12341223".U, "x124".U), //sd
          List("xabc41223".U, "xFFFFFFFFFFFFFAA4".U), //sd
          List("x02341263".U, "x12".U), //beq
          List("x92341263".U, "xFFFFFFFFFFFFF892".U), //beq 
          List("xabc41263".U, "xFFFFFFFFFFFFF952".U), //beq
          List("x12341213".U, "x123".U), //addi
        )
        tests.foreach((t) => {
          poke(dut.io.insn, t(0))
          expect(dut.io.imm, t(1), msg=s"input: ${t(0).toString(16)}")
        })
      }
    } should be (true)
  }
}

class ALUControlTest extends FlatSpec with Matchers {
  "ImmGen" should "" in {
    chisel3.iotesters.Driver( () => new ALUControl()) { dut =>
      new PeekPokeTester(dut) {
        val tests = List(
          List(AluOp.ld, AluCtrl.add)
          ,List(AluOp.sd, AluCtrl.add)
          ,List(AluOp.beq, AluCtrl.sub)
          ,List(AluOp.reg, AluCtrl.add)
        )
        tests.foreach((t) => {
          poke(dut.io.aluOp, t(0))
          poke(dut.io.funct3, 0.U)
          poke(dut.io.funct7, 0.U)
          expect(dut.io.aluCtrl, t(1), msg=s"input: ${t(0).toString(16)}")
        })
      }
    } should be (true)
  }
}
class ControlTest extends FlatSpec with Matchers {
  "ImmGen" should "" in {
    chisel3.iotesters.Driver( () => new Control()) { dut =>
      new PeekPokeTester(dut) {
          var inputStr = "addi"
          poke(dut.io.in, OpCode.addi)
          expect(dut.io.write_reg,      true.B, msg=s"input: ${inputStr}")
          expect(dut.io.aluSrcFromReg,  false.B, msg=s"input: ${inputStr}")
          expect(dut.io.memWrite,       false.B, msg=s"input: ${inputStr}")
          expect(dut.io.memToReg,       false.B, msg=s"input: ${inputStr}")
          expect(dut.io.aluOp,          AluOp.reg, msg=s"input: ${inputStr}")
          expect(dut.io.branch,         false.B, msg=s"input: ${inputStr}")

          inputStr = "add"
          poke(dut.io.in, OpCode.add)
          expect(dut.io.write_reg,      true.B, msg=s"input: ${inputStr}")
          expect(dut.io.aluSrcFromReg,  true.B, msg=s"input: ${inputStr}")
          expect(dut.io.memWrite,       false.B, msg=s"input: ${inputStr}")
          expect(dut.io.memToReg,       false.B, msg=s"input: ${inputStr}")
          expect(dut.io.aluOp,          AluOp.reg, msg=s"input: ${inputStr}")
          expect(dut.io.branch,         false.B, msg=s"input: ${inputStr}")

          inputStr = "sd"
          poke(dut.io.in, OpCode.sd)
          expect(dut.io.write_reg,      false.B, msg=s"input: ${inputStr}")
          expect(dut.io.aluSrcFromReg,  false.B, msg=s"input: ${inputStr}")
          expect(dut.io.memWrite,       true.B, msg=s"input: ${inputStr}")
          //expect(dut.io.memToReg,       false.B, msg=s"input: ${inputStr}")
          expect(dut.io.aluOp,          AluOp.sd, msg=s"input: ${inputStr}")
          expect(dut.io.branch,         false.B, msg=s"input: ${inputStr}")

          inputStr = "ld"
          poke(dut.io.in, OpCode.ld)
          expect(dut.io.write_reg,      true.B, msg=s"input: ${inputStr}")
          expect(dut.io.aluSrcFromReg,  false.B, msg=s"input: ${inputStr}")
          expect(dut.io.memWrite,       false.B, msg=s"input: ${inputStr}")
          expect(dut.io.memToReg,       true.B, msg=s"input: ${inputStr}")
          expect(dut.io.aluOp,          AluOp.ld, msg=s"input: ${inputStr}")
          expect(dut.io.branch,         false.B, msg=s"input: ${inputStr}")

          inputStr = "beq"
          poke(dut.io.in, OpCode.beq)
          expect(dut.io.write_reg,      false.B, msg=s"input: ${inputStr}")
          expect(dut.io.aluSrcFromReg,  false.B, msg=s"input: ${inputStr}")
          expect(dut.io.memWrite,       false.B, msg=s"input: ${inputStr}")
          //expect(dut.io.memToReg,       false.B, msg=s"input: ${inputStr}")
          expect(dut.io.aluOp,          AluOp.beq, msg=s"input: ${inputStr}")
          expect(dut.io.branch,         true.B, msg=s"input: ${inputStr}")
      }
    } should be (true)
  }
}