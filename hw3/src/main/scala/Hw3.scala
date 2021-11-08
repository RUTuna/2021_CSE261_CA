
package Hw3
import chisel3._
import chisel3.util._
import chisel3.iotesters.PeekPokeTester
import firrtl.FirrtlProtos.Firrtl.Expression.PrimOp.Op


object OpCode {
  val aluImm   = "b0010011".U
  val aluReg   = "b0110011".U
  val store    = "b0100011".U
  val load     = "b0000011".U
  val branch   = "b1100011".U
  val jal      = "b1101111".U
  val jalr     = "b1100111".U
}

class Control extends Module {
  val io = IO(new Bundle{
    val in = Input(UInt(7.W))
    val write_reg = Output(Bool())
    val aluSrcFromReg = Output(Bool())
    val memWrite = Output(Bool())
    val memRead = Output(Bool())
    val memToReg = Output(Bool())
    val aluOp = Output(UInt(2.W))
    val branch = Output(Bool())
    val link = Output(Bool())
    val jal = Output(Bool())
    val indir = Output(Bool())
  })

  io.aluOp := false.B

  io.write_reg := false.B

  io.jal := false.B
  io.link := false.B
  io.indir := false.B



  io.aluSrcFromReg := false.B

  io.memWrite := 0.U

  io.memToReg := 0.U

  io.branch := 0.U

  io.memRead := 0.U

  
}


class RegFile extends Module {
  val io = IO(new Bundle {
    val rd = Input(UInt(5.W))
    val rs1 = Input(UInt(5.W))
    val rs2 = Input(UInt(5.W))
    val write = Input(Bool())
    val wdata = Input(UInt(64.W))
    val rs1_out= Output(UInt(64.W))
    val rs2_out = Output(UInt(64.W))
  })

  /* Your code starts here */

  io.rs1_out := 0.U
  io.rs2_out := 0.U

  /* Your code ends here */


}



class Decoder extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val opcode = Output(UInt(7.W))
    val rd = Output(UInt(5.W))
    val funct3 = Output(UInt(3.W))
    val funct7 = Output(UInt(7.W))
    val rs1 = Output(UInt(5.W))
    val rs2 = Output(UInt(5.W))
  })
  /* Your code starts here */
 
  io.opcode := 0.U

  io.rd := 0.U


  io.funct3 := 0.U
  io.rs1 := 0.U
  io.rs2 := 0.U
  
  io.funct7 := 0.U
   /* Your code ends here */


}

object AluOp {
  val ld = "b00".U
  val sd = "b01".U
  val beq = "b10".U
  val reg = "b11".U
}

class ALUControl extends Module {
  val io = IO(new Bundle {
    val aluOp = Input(UInt(2.W))
    val funct3 = Input(UInt(3.W))
    val funct7 = Input(UInt(7.W))
    val aluCtrl = Output(UInt(4.W))
  })

  /* Your code starts here */
  io.aluCtrl := 0.U
  /* Your code ends here */

}


class ImmGen extends Module {
  val io = IO(new Bundle {
    val insn = Input(UInt(32.W))
    val imm = Output(UInt(64.W))
  })

  /* Your code starts here */

  io.imm := 0.U
  /* Your code ends here */

}


object AluCtrl {
  val and = "b0000".U(4.W)
  val or  = "b0001".U(4.W)
  val add = "b0010".U(4.W)
  val sub = "b0110".U(4.W)
}

class ALU extends Module {
  val io = IO(new Bundle {
    val ctrl = Input(UInt(4.W))
    val a = Input(UInt(64.W))
    val b = Input(UInt(64.W))
    val res = Output(UInt(64.W))
    val zero = Output(Bool())
  })

  /* Your code starts here */

  io.res := 0.U
  io.zero := 0.U
  
  /* Your code ends here */


}



class PCGen extends Module {
  val io = IO(new Bundle{
    val this_pc = Input(UInt(64.W))
    val branch = Input(Bool())
    val jal = Input(Bool())
    val indir = Input(Bool())
    val zero = Input(Bool())
    val rs1  = Input(UInt(64.W))
    val next_pc = Output(UInt(64.W))
    val imm64 = Input(UInt(64.W))
  })

  /* Your code starts here */
  io.next_pc :=  0.U
  /* Your code ends here */
  

}


class Core extends Module   {
  val io = IO(new Bundle {
    //val reset = Input(Bool())
    val imem_addr = Output(UInt(64.W))
    val imem_insn = Input(UInt(32.W))
    val dmem_addr = Output(UInt(64.W))
    val dmem_write = Output(Bool())
    val dmem_read = Output(Bool())
    val dmem_wdata = Output(UInt(64.W))
    val dmem_rdata = Input(UInt(64.W))

    
    val halted = Output(Bool())

  })

  /* Support for halt, don't touch start */
  val started = RegInit(false.B)
  started := true.B
  val halted = RegInit(false.B)
  when(io.imem_insn === "x00005073".U) {
    //printf("halting\n")
    halted := true.B
  }.otherwise {
    halted := halted
  }
  io.halted := halted
  /* Support for halt, don't touch end */

  /* Modules, don't touch start */

  val pc = RegInit(0.U(64.W))
  val control = Module(new Control())
  val pcGen = Module(new PCGen())
  val decoder = Module(new Decoder())
  val aluControl = Module(new ALUControl())
  val regfile = Module(new RegFile())
  val alu = Module(new ALU())
  val immGen = Module(new ImmGen())
  pc := Mux(started, pcGen.io.next_pc, pc)

  /* Modules, don't touch end */

  /* Your code starts here */





  io.dmem_read := false.B

  pcGen.io.branch := 0.U
  pcGen.io.this_pc := 0.U
  pcGen.io.indir := 0.U
  
  // Fetch

  
  
  
  io.imem_addr := 0.U
  
  

  // Deocder
  
  
  decoder.io.in := io.imem_insn
  control.io.in := 0.U
  
  aluControl.io.funct3 := 0.U
  aluControl.io.funct7 := 0.U
  aluControl.io.aluOp := 0.U

  
  immGen.io.insn := 0.U
  pcGen.io.imm64 := 0.U

  
  regfile.io.rs1 := 0.U
  regfile.io.rs2 := 0.U
  regfile.io.rd := 0.U
  
  regfile.io.write := false.B
  

  


  // EX
  
  alu.io.ctrl := 0.U
  alu.io.a := 0.U
  alu.io.b := 0.U

  
  pcGen.io.zero := false.B
  pcGen.io.rs1 := 0.U
  pcGen.io.jal := false.B

  // MEM

  io.dmem_addr := 0.U
  io.dmem_write := false.B
  io.dmem_read := false.B
  io.dmem_wdata := false.B



  


  // WB

  regfile.io.wdata := 0.U

  /* Your code endshere */

  // Logs for debugging, freely modify
  printf("(pc: %x, instruction: %x, next_pc: %x, wdata: %x, write: %x)\n",
  pc, io.imem_insn, pcGen.io.next_pc, regfile.io.wdata, regfile.io.write)

}












