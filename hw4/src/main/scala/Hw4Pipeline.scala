
package UniRV64I
import chisel3._
import chisel3.util._
import chisel3.iotesters.PeekPokeTester


class IFID extends Bundle {
  val pc = UInt(64.W)
  val insn = UInt(32.W)
  val valid = Bool()
  
  
  override def toPrintable: Printable = {
    p"IFID: (${Hexadecimal(pc)}, ${Hexadecimal(insn)}, ${valid})"
  }
}

class IFIDReg extends Module {
  val io = IO(new Bundle{
    val in = Input(new IFID())
    val stall = Input(Bool())
    val out = Output(new IFID())
  })
  
  val reg = Reg(new IFID())
  reg := io.in
  io.out := reg
  val valid_reg = RegInit(false.B)
  valid_reg := io.in.valid
  io.out.valid := Mux(io.stall, false.B, valid_reg)
}

class IDEX extends Bundle {
  val debug_insn = UInt(32.W)
  val pc = UInt(64.W)
  val rs1_data = UInt(64.W)
  val rs2_data = UInt(64.W)
  val imm64 = UInt(64.W)
  val rd = UInt(5.W)
  val funct3 = UInt(3.W)
  val funct7 = UInt(7.W)
  val control = new Control()
  val valid = Bool()
  
  override def toPrintable: Printable = {
    (p"IDEX: (${Hexadecimal(pc)}, ${Hexadecimal(debug_insn)}, ${valid})" + 
    p" (${control})" +
    p" (${rs1_data}, ${rs2_data}, ${imm64}, ${rd}, ${funct3}, ${funct7})"
    )
  }
}

class IDEXReg extends Module {
  val io = IO(new Bundle {
    val in = Input(new IDEX())
    val out = Output(new IDEX())
  })
  val reg = Reg(new IDEX())
  
  reg := io.in
  io.out := reg
  val valid_reg = RegInit(false.B)
  valid_reg := io.in.valid
  io.out.valid := valid_reg
}

class EXMEM extends Bundle {
  val debug_insn = UInt(32.W)
  val pc = UInt(64.W)
  val control = new Control()
  val rd = UInt(5.W)
  val branch_target = UInt(64.W)
  val branch_taken = Bool()
  val alu_res = UInt(64.W)
  val rs2_data = UInt(64.W)
  val valid = Bool()
  
  override def toPrintable: Printable = {
    (p"EXMEM: (${Hexadecimal(pc)}, ${Hexadecimal(debug_insn)}, ${valid})" + 
    p" (${control})" +
    p" (${rd}, ${branch_target}, ${branch_taken}, ${alu_res}, ${rs2_data})"
    )
  }
}

class EXMEMReg extends Module {
  val io = IO(new Bundle {
    val in = Input(new EXMEM())
    val out = Output(new EXMEM())
  })
  val reg = Reg(new EXMEM())
  reg := io.in
  io.out := reg
  val valid_reg = RegInit(false.B)
  valid_reg := io.in.valid
  io.out.valid := valid_reg
}

class MEMWB extends Bundle {
  val pc = UInt(64.W)
  val debug_insn = UInt(32.W)
  val control = new Control()
  val rd = UInt(5.W)
  val alu_res = UInt(64.W)
  val valid = Bool()
  
  override def toPrintable: Printable = {
    (p"MEMWB: (${Hexadecimal(pc)}, ${Hexadecimal(debug_insn)}, ${valid})" + 
    p" (${control})" +
    p" (${rd}, ${alu_res})"
    )
  }
}
class MEMWBReg extends Module {
  val io = IO(new Bundle {
    val in = Input(new MEMWB())
    val out = Output(new MEMWB())
  })
  val reg = Reg(new MEMWB())
  reg := io.in
  io.out := reg
  val valid_reg = RegInit(false.B)
  valid_reg := io.in.valid
  io.out.valid := valid_reg
}


class Control extends Bundle {
  val write_reg = Bool()
  val aluSrcFromReg = Bool()
  val memWrite = Bool()
  val memRead = Bool()
  val memToReg = Bool()
  val aluOp = UInt(2.W)
  val branch = Bool()
  val link = Bool()
  val jal = Bool()
  val indir = Bool()
  val stall = Bool()
  override def toPrintable: Printable = {
    p"($write_reg, $aluSrcFromReg, $memWrite, $memRead;" +
    p" $memToReg, $aluOp, $branch, $link;" +
    p" $jal, $indir, $stall)"
  }
}

