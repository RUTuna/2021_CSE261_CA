package UniCache
import chisel3._
import chisel3.util._

/* 
  Take the piplined approach to avoid using finite state machine
  Stages: Request - Tag Read - Mem Access - ...
  
  8 * 64 = 512 bits per line
  4 lines per set = 2048 bits = 256 bytes per set
  cache size = 1KB (4 sets)
  | tag (58 bits) | index (2 bits) | offset (4bits) |
*/

class UniCache() extends Module  {
  val io = IO(new Bundle {
    val req = Input(new UniCacheReq())
    val req_ready = Output(Bool())
    val resp = Output(new UniCacheResp())
    val mem_req = Output(new MemReq())
    val mem_resp = Input(new MemResp())

    val debug_clear = Input(UInt(4.W))
    val debug_valid = Input(Bool())
  })

  // State machine 
  val s_idle :: s_compare_tag :: s_allocate :: s_wait :: s_write_back :: Nil = Enum(5)
  val state = RegInit(s_idle)

  // Registers to keep data
  
  val req_reg = Reg(new UniCacheReq())
  val tag_reg = Reg(UInt(58.W))
  val data_reg = Reg(UInt(256.W))
  val valid_reg = Reg(Bool())

  // wires from / to cache memory
  val rindex = Wire(UInt(2.W))
  val fromTagArray = Wire(UInt(58.W))
  val fromValidArray = Wire(Bool())
  val fromDataArray = Wire(UInt(256.W))

  val cache_write = Wire(Bool())
  val windex = Wire(UInt(2.W))
  val wtag = Wire(UInt(58.W))
  val wvalid = Wire(Bool())
  val wdata = Wire(UInt(256.W))

  ///////////////////////////
  /* Your code starts here */
  ///////////////////////////

  // idle state
  rindex := 0.U
  
  

  // compare state
  
  // write back state
  cache_write := false.B
  windex := 0.U
  wtag := 0.U
  wvalid := false.B
  wdata := 0.U
  

  io.mem_req.waddr := 0.U
  io.mem_req.write := false.B
  io.mem_req.wdata := 0.U

  // allocate state

  
  io.mem_req.raddr := Cat(req_reg.addr(63,4), 0.U(4.W))
  io.mem_req.read := state === s_allocate

  // wait state
  
  
  
  
  io.req_ready := false.B


  // resp gen

  val rdata_reg = Reg(UInt(64.W))
  
  io.resp.valid := false.B
  io.resp.rdata := 0.U
  
  /////////////////////////
  /* Your code ends here */
  /////////////////////////

  // Cache Arrays
  
  val tagArray = SyncReadMem(4, UInt(58.W))
  val validArray = SyncReadMem(4, Bool())
  val dataArray = SyncReadMem(4, UInt(256.W))


  fromTagArray := tagArray.read(rindex)
  fromValidArray := validArray.read(rindex)
  fromDataArray := dataArray.read(rindex)

  when(io.debug_valid) {
    tagArray.write(io.debug_clear, 0.U)
    dataArray.write(io.debug_clear, 0.U)
    validArray.write(io.debug_clear, false.B)
  }.elsewhen(cache_write) {
    tagArray.write(windex, wtag)
    dataArray.write(windex, wdata)
    validArray.write(windex, wvalid)
  }
  


  // Logging

  val cycle = RegInit(0.U(64.W))
  cycle := Mux(io.debug_valid, 0.U, cycle + 1.U)
  when(!io.debug_valid) {
    printf(p"$cycle: (state: $state)\n")
  }
  
  
  


}


