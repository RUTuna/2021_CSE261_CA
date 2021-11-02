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
  val sd = "b01".U
  val beq = "b10".U
  val reg = "b11".U
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

