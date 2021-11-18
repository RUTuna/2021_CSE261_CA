
package UniRV64I
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

class ControlModule extends Module {
  val io = IO(new Bundle{
    val in = Input(UInt(7.W))
    val out = Output(new Control())
  })

  io.out.aluOp := 0.U
  io.out.write_reg := 0.U
  io.out.jal := 0.U
  io.out.link := 0.U
  io.out.indir := 0.U
  io.out.aluSrcFromReg := 0.U
  io.out.memWrite := 0.U
  io.out.memToReg := 0.U
  io.out.branch := 0.U
  io.out.memRead := 0.U
  io.out.stall := 0.U
  
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

  val registers = RegInit(VecInit(Seq.fill(32)(0.U(64.W))))

  io.rs1_out := 0.U
  io.rs2_out := 0.U


  printf(p"Register content: ${registers}\n")


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
    val imm12 = Output(UInt(12.W))
  })

  io.opcode := 0.U
  io.rd := 0.U
  io.funct3 := 0.U
  io.rs1 := 0.U
  io.rs2 := 0.U
  io.imm12 := 0.U
  io.funct7 := 0.U


}

object AluOp {
  val ld = "b00".U
  val sd = "b00".U
  val beq = "b01".U
  val reg = "b10".U
}

class ALUControl extends Module {
  val io = IO(new Bundle {
    val aluOp = Input(UInt(2.W))
    val funct3 = Input(UInt(3.W))
    val funct7 = Input(UInt(7.W))
    val aluCtrl = Output(UInt(4.W))
  })

  io.aluCtrl := 0.U

}


class ImmGen extends Module {
  val io = IO(new Bundle {
    val insn = Input(UInt(32.W))
    val imm = Output(UInt(64.W))
  })

  io.imm := 0.U

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


  io.res := 0.U
  io.zero := 0.U


}







class Core extends Module {
  val io = IO(new Bundle {
    //val reset = Input(Bool())
    val imem_addr = Output(UInt(64.W))
    val imem_insn = Input(UInt(32.W))
    val dmem_addr = Output(UInt(64.W))
    val dmem_write = Output(Bool())
    val dmem_read = Output(Bool())
    val dmem_wdata = Output(UInt(64.W))
    val dmem_rdata = Input(UInt(64.W))

    val wb_insn = Output(UInt(32.W))
    val halted = Output(Bool())

  })

  // pipeline registers

  val ifid_reg = Module(new IFIDReg())
  val idex_reg = Module(new IDEXReg())
  val exmem_reg = Module(new EXMEMReg())
  val memwb_reg = Module(new MEMWBReg())

  // combinational modules
  val control = Module(new ControlModule())
  val decoder = Module(new Decoder())
  val immGen = Module(new ImmGen())
  val regfile = Module(new RegFile())
  val aluControl = Module(new ALUControl())
  val alu = Module(new ALU())

  // support for halts
  val halted = RegInit(false.B)
  when(exmem_reg.io.out.debug_insn === "x00005073".U) {
    //printf("halting\n")
    halted := true.B
  }.otherwise {
    halted := halted
  }
  io.halted := halted

  // misc. skeleton
  val next_pc = Wire(UInt(64.W))
  val started = RegInit(true.B)
  started := false.B
  val pc = RegInit(0.U(64.W))
  io.imem_addr := Mux(started, 0.U, pc)
  pc := Mux(started, pc, next_pc)

  /* Your code from here */


  io.dmem_read := 0.U

  // IF Stage
  
  
  ifid_reg.io.stall := 0.U
  
  
  next_pc := 0.U
    
  // Fetch

  ifid_reg.io.in.pc := 0.U
  ifid_reg.io.in.insn := 0.U
  ifid_reg.io.in.valid := 0.U

  // ID Stage

  
  
  
  decoder.io.in := 0.U
  control.io.in := 0.U

  
  immGen.io.insn := 0.U

  
  regfile.io.rs1 := 0.U
  regfile.io.rs2 := 0.U
  regfile.io.rd := 0.U
  
  regfile.io.write := 0.U

  idex_reg.io.in.pc := 0.U
  idex_reg.io.in.rs1_data := 0.U
  idex_reg.io.in.rs2_data := 0.U
  idex_reg.io.in.imm64 := 0.U
  idex_reg.io.in.rd := 0.U
  idex_reg.io.in.control := control.io.out
  idex_reg.io.in.funct3 := 0.U
  idex_reg.io.in.funct7 := 0.U
  idex_reg.io.in.debug_insn := 0.U
  idex_reg.io.in.valid :=  0.U
//!exmem_reg.io.out.branch_taken &

  

  


  // EX
  


  
  aluControl.io.funct3 := 0.U
  aluControl.io.funct7 := 0.U
  aluControl.io.aluOp := 0.U
  
  
  
  alu.io.ctrl := 0.U
  alu.io.a := 0.U
  alu.io.b := 0.U

  


  exmem_reg.io.in.branch_target := 0.U
  exmem_reg.io.in.branch_taken := 0.U

  exmem_reg.io.in.alu_res := 0.U
  exmem_reg.io.in.rs2_data := 0.U
  exmem_reg.io.in.control := idex_reg.io.out.control
  exmem_reg.io.in.pc := 0.U
  exmem_reg.io.in.debug_insn := 0.U
  exmem_reg.io.in.valid :=  0.U
  exmem_reg.io.in.rd := 0.U

  // MEM

  io.dmem_addr := 0.U
  io.dmem_write := 0.U
  io.dmem_read := 0.U
  io.dmem_wdata := 0.U

  

  memwb_reg.io.in.alu_res := 0.U
  memwb_reg.io.in.control := exmem_reg.io.out.control
  memwb_reg.io.in.debug_insn := 0.U
  memwb_reg.io.in.pc := 0.U
  memwb_reg.io.in.valid := 0.U
  memwb_reg.io.in.rd := 0.U
  



  


  // WB

  regfile.io.wdata := 0.U
  

  /* Your code to here */
  // Logs and traces
  io.wb_insn := Mux(memwb_reg.io.out.valid, memwb_reg.io.out.debug_insn, 0.U)
  val log_cycles = RegInit(0.U(64.W))
  log_cycles := log_cycles + 1.U
  printf("--------------------------------------------------------------\n")
  printf("--------------------------------------------------------------\n")
  printf("Log @ %d\n", log_cycles)
  printf(p"${ifid_reg.io.out}\n")
  printf(p"${idex_reg.io.out}\n")
  printf(p"${exmem_reg.io.out}\n")
  printf(p"${memwb_reg.io.out}\n")
  printf("--------------------------------------------------------------\n")

}












