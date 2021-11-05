/*
 * Homework 2, CSE261 Computer Architecture 
 * 2021 Fall
 * UNIST
 * Hyungon Moon
 */

package Hw2
import chisel3._
import chisel3.util._


object OpCode {
  val addi  = "b0010011".U
  val add   = "b0110011".U
  val sd    = "b0100011".U
  val ld    = "b0000011".U
  val beq   = "b1100011".U
}

object AluOp {
  val ld = "b00".U
  val sd = "b00".U
  val beq = "b01".U
  val reg = "b10".U
}

object AluCtrl {
  val and = "b0000".U(4.W)
  val or  = "b0001".U(4.W)
  val add = "b0010".U(4.W)
  val sub = "b0110".U(4.W)
}

/*
Task 1: ALU
*/

class ALU extends Module {
  val io = IO(new Bundle {
    val ctrl = Input(UInt(4.W))
    val a = Input(UInt(64.W))
    val b = Input(UInt(64.W))
    val res = Output(UInt(64.W))
    val zero = Output(Bool())
  })
  
  /* Your code starts here*/
  io.res := 0.U
  io.zero := true.B

  switch(io.ctrl){
    is(AluCtrl.and) {io.res := io.a & io.b}
    is(AluCtrl.or) {io.res := io.a | io.b}
    is(AluCtrl.add) {io.res := io.a + io.b}
    is(AluCtrl.sub) {io.res := io.a - io.b}
  }
  /*Your code ends here */

}

/*
Task 2: ImmGen
*/


class ImmGen extends Module {
  val io = IO(new Bundle {
    val insn = Input(UInt(32.W))
    val imm = Output(UInt(64.W))
  })

  /* Your code starts here*/
  io.imm := 0.U

  switch(io.insn(6,0)){
    is(OpCode.beq) {
      val back = Cat(io.insn(30,25), io.insn(11,8))
      val front = Cat(io.insn(31,31), io.insn(7,7))
      io.imm := Cat(front, back)
    } 
    is(OpCode.ld) { 
      io.imm := io.insn(31,20)
    } 
    is(OpCode.sd) {
      io.imm := Cat(io.insn(31,25), io.insn(11,7))
    }
    is(OpCode.add) {
      io.imm := io.insn
    }
    is(OpCode.addi) {
      io.imm := io.insn(31,20)
    }
  }
  /*Your code ends here */

}

/*
Task 3: ALUControl
*/

class ALUControl extends Module {
  val io = IO(new Bundle {
    val aluOp = Input(UInt(2.W))
    val funct3 = Input(UInt(3.W))
    val funct7 = Input(UInt(7.W))
    val aluCtrl = Output(UInt(4.W))
  })

  /* Your code starts here*/
  io.aluCtrl := 0.U

  when(io.aluOp === 0.U) {
    io.aluCtrl := AluCtrl.add
  } .elsewhen (io.aluOp === 1.U) {
    io.aluCtrl := AluCtrl.sub
  } .otherwise { 
    when(io.funct3 === 0.U){
      when(io.funct7 === 0.U) {
        io.aluCtrl := AluCtrl.add
      } .otherwise {
        io.aluCtrl := AluCtrl.sub
      }
    } .otherwise {
      when(io.funct3(0,0) === 0.U) {
        io.aluCtrl := AluCtrl.or
      } .otherwise {
        io.aluCtrl := AluCtrl.and
      }
    }
  }
  /*Your code ends here */

}

/*
Task 4: Control
*/

class Control extends Module {
  val io = IO(new Bundle{
    val in = Input(UInt(7.W))
    val write_reg = Output(Bool())
    val aluSrcFromReg = Output(Bool())
    val memWrite = Output(Bool())
    val memToReg = Output(Bool())
    val aluOp = Output(UInt(2.W))
    val branch = Output(Bool())
  })
/* Your code starts here*/
    io.aluOp := 0.U

  io.write_reg := false.B

  io.aluSrcFromReg := false.B

  io.memWrite := false.B

  io.memToReg := false.B

  io.branch := false.B


  /*Your code ends here */

  
}

