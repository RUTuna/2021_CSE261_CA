
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

  io.jal := false.B
  io.link := false.B
  io.indir := false.B

  io.memRead := false.B

  io.aluOp := 0.U
  io.write_reg := false.B
  io.aluSrcFromReg := false.B
  io.memWrite := false.B
  io.memToReg := false.B
  io.branch := false.B

  switch(io.in) {
    is(OpCode.branch) {
      io.write_reg := false.B
      io.aluSrcFromReg := false.B
      io.memWrite := false.B
      io.memRead := false.B
      // io.memToReg := false.B
      io.aluOp := AluOp.beq
      io.branch := true.B
    } 
    is(OpCode.load) { 
      io.aluOp := AluOp.ld
      io.write_reg := true.B
      io.aluSrcFromReg := true.B
      io.memWrite := false.B
      io.memRead := true.B
      io.memToReg := true.B
      io.branch := false.B
    } 
    is(OpCode.store) {
      io.aluOp := AluOp.sd
      io.write_reg := false.B
      io.aluSrcFromReg := true.B
      io.memWrite := true.B
      io.memRead := false.B
      // io.memToReg := true.B
      io.branch := false.B
    }
    is(OpCode.aluReg) {
      io.aluOp := AluOp.reg
      io.write_reg := true.B
      io.aluSrcFromReg := false.B
      io.memWrite := false.B
      io.memRead := false.B
      io.memToReg := false.B
      io.branch := false.B
    }
    is(OpCode.aluImm) {
      io.aluOp := AluOp.reg
      io.write_reg := true.B
      io.aluSrcFromReg := true.B
      io.memWrite := false.B
      io.memRead := false.B
      io.memToReg := false.B
      io.branch := false.B
    }
  }
  
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
  val registers = Reg(Vec(64, UInt(64.W)))
  registers(0) := 0.U
  io.rs1_out := Mux(io.rs1.orR, registers(io.rs1), 0.U)
  io.rs2_out := Mux(io.rs2.orR, registers(io.rs2), 0.U)

  when(io.write && io.rd.orR){
    registers(io.rd) := io.wdata
  }
  printf("[Regfile] rd: %x, rs1: %x, rs2: %x, rs1_out: %x, rs2_out: %x, wdata: %x\n", io.rd, io.rs1, io.rs2, io.rs1_out, io.rs2_out, io.wdata)
 
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
 
  io.opcode := io.in(6,0)

  switch(io.opcode){
    is(OpCode.aluImm){printf("[Decoder] aluImm\n")}
    is(OpCode.aluReg){printf("[Decoder] aluReg\n")}
    is(OpCode.store){printf("[Decoder] store\n")}
    is(OpCode.load){printf("[Decoder] load\n")}
    is(OpCode.branch){printf("[Decoder] branch\n")}
  }

  io.rd := io.in(11,7)


  io.funct3 := io.in(14,12)
  io.rs1 := io.in(19,15)
  io.rs2 := io.in(24,20)
  
  io.funct7 := io.in(31,25)
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

  when(io.aluOp === AluOp.ld || io.aluOp === AluOp.sd) {
    io.aluCtrl := AluCtrl.add
  } .elsewhen (io.aluOp === AluOp.beq) {
    io.aluCtrl := AluCtrl.sub
  } .otherwise { 
    when(io.funct3 === 0.U){
      when(io.funct7 === "b0100000".U) {
        io.aluCtrl := AluCtrl.sub
      } .otherwise {
        io.aluCtrl := AluCtrl.add
      }
    } .otherwise {
      when(io.funct3(0,0) === 0.U) {
        io.aluCtrl := AluCtrl.or
      } .otherwise {
        io.aluCtrl := AluCtrl.and
      }
    }
  }
  /* Your code ends here */

}


class ImmGen extends Module {
  val io = IO(new Bundle {
    val insn = Input(UInt(32.W))
    val imm = Output(UInt(64.W))
  })

  /* Your code starts here */
  io.imm := 0.U
  when(io.insn(31,31) === 1.U){
    val nega = "xFFFFFFFFFFFFF".U
    switch(io.insn(6,0)){
      is(OpCode.branch) {
        val back = Cat(io.insn(30,25), io.insn(11,8))
        val front = Cat(io.insn(31,31), io.insn(7,7))
        val test = Cat(front, back)
        io.imm := Cat(nega, test)
      } 
      is(OpCode.load) { 
        io.imm := Cat(nega, io.insn(31,20)) 
      } 
      is(OpCode.store) {
        val test = Cat(io.insn(31,25), io.insn(11,7))
        io.imm :=  Cat(nega, test)
      }
      is(OpCode.aluReg) {
        io.imm := Cat(nega, io.insn) 
      }
      is(OpCode.aluImm) {
        io.imm := Cat(nega, io.insn(31,20))
      }
    }
  } .otherwise {
    switch(io.insn(6,0)){
      is(OpCode.branch) {
        val back = Cat(io.insn(30,25), io.insn(11,8))
        val front = Cat(io.insn(31,31), io.insn(7,7))
        io.imm := Cat(front, back)
      } 
      is(OpCode.load) { 
        io.imm := io.insn(31,20)
      } 
      is(OpCode.store) {
        io.imm := Cat(io.insn(31,25), io.insn(11,7))
      }
      is(OpCode.aluReg) {
        io.imm := io.insn
      }
      is(OpCode.aluImm) {
        io.imm := io.insn(31,20)
      }
    }
  }
  /* Your code ends here */
  printf("[ImmGen] imm: %x\n", io.imm)
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
  io.zero := false.B

  switch(io.ctrl){
    is(AluCtrl.and) {io.res := io.a & io.b}
    is(AluCtrl.or) {io.res := io.a | io.b}
    is(AluCtrl.add) {io.res := io.a + io.b}
    is(AluCtrl.sub) {
      when(io.a - io.b === 0.U){
        io.zero := true.B
      } 
      io.res := io.a - io.b
    }
  }
  printf("[ALU] ctrl: %x, a: %x, b: %x, res: %x\n", io.ctrl, io.a, io.b, io.res)
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
  val sum = io.this_pc + io.imm64 * 2.U
  val add = io.branch && io.zero
  io.next_pc := Mux(io.branch && io.zero, sum, io.this_pc + 4.U)
  // io.this_pc + 4.U
  // Mux(io.branch && io.zero, sum, io.this_pc + 4.U)
  /* Your code ends here */
  
  printf("[PCGen] next_pc: %x\n", io.next_pc)
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

  pcGen.io.branch := control.io.branch
  pcGen.io.this_pc := pc
  pcGen.io.indir := 0.U
  
  // Fetch

  
  
  
  io.imem_addr := pcGen.io.this_pc
  
  

  // Deocder
  
  
  decoder.io.in := io.imem_insn
  control.io.in := decoder.io.opcode
  
  aluControl.io.funct3 := decoder.io.funct3
  aluControl.io.funct7 := decoder.io.funct7
  aluControl.io.aluOp := control.io.aluOp

  
  immGen.io.insn := io.imem_insn
  pcGen.io.imm64 := immGen.io.imm

  
  regfile.io.rs1 := decoder.io.rs1
  regfile.io.rs2 := decoder.io.rs2
  regfile.io.rd := decoder.io.rd
  
  regfile.io.write := control.io.write_reg
  

  


  // EX
  
  alu.io.ctrl := aluControl.io.aluCtrl
  alu.io.a := regfile.io.rs1_out
  alu.io.b := Mux(control.io.aluSrcFromReg, immGen.io.imm, regfile.io.rs2_out)

  
  pcGen.io.zero := alu.io.zero
  pcGen.io.rs1 := 0.U
  pcGen.io.jal := false.B

  // MEM

  when(decoder.io.opcode === OpCode.load || decoder.io.opcode === OpCode.store){
    io.dmem_addr := alu.io.res
  } .otherwise {
    io.dmem_addr := 0.U
  }
  // io.dmem_addr := alu.io.res
  io.dmem_write := control.io.memWrite
  io.dmem_read := control.io.memRead
  io.dmem_wdata := regfile.io.rs2_out



  


  // WB

  regfile.io.wdata := Mux(control.io.memToReg, io.dmem_rdata, alu.io.res)

  /* Your code endshere */

  // Logs for debugging, freely modify
  printf("(pc: %x, instruction: %x, aluSrcFromReg: %x, next_pc: %x, wdata: %x, write: %x, dmem_addr: %x, imem_addr: %x)\n",
  pc, io.imem_insn, control.io.aluSrcFromReg, pcGen.io.next_pc, regfile.io.wdata, regfile.io.write, io.dmem_addr, io.imem_addr)
  // printf("%x [io_out]imem_addr: %x, dmem_addr: %x, dmem_write: %x, dmem_read: %x, dmem_wdata: %x \n", pc, io.imem_addr, io.dmem_addr, io.dmem_write, io.dmem_read, io.dmem_wdata)

}












