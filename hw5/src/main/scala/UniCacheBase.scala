package UniCache
import chisel3._
import chisel3.util._




class UniCacheReq() extends Bundle {
  val addr = UInt(64.W)
  val wdata = UInt(64.W)
  val write = Bool()
  val valid = Bool()
  override def toPrintable: Printable = {
    p"Request: (${valid}, ${write}, ${Hexadecimal(addr)}, ${Hexadecimal(wdata)})"
  }
}

class UniCacheResp() extends Bundle {
  val rdata = UInt(64.W)
  val valid = Bool()
  override def toPrintable: Printable = {
    p"Response: (${valid}, ${Hexadecimal(rdata)})"
  }
}


class MemReq extends Bundle {
  val raddr = UInt(64.W)
  val waddr = UInt(64.W)
  val wdata = UInt(256.W)
  val write = Bool()
  val read = Bool()
}

class MemResp extends Bundle {
  val rdata = UInt(256.W)
  val valid = Bool()
}


class UniCacheBase extends Module {
  val io = IO(new Bundle {
    val onInit = Input(Bool())
    val debug_clear = Input(UInt(4.W))
    val debug_clear_mem_addr = Input(UInt(32.W))
    val debug_clear_mem_data = Input(UInt(256.W))
    val debug_valid = Input(Bool())
    val req = Input(new UniCacheReq())
    val req_ready = Output(Bool())
    val resp = Output(new UniCacheResp())
    
  })

  val cache = Module(new UniCache())
  cache.io.req := io.req
  io.req_ready := cache.io.req_ready
  io.resp := cache.io.resp
  cache.io.debug_clear := io.debug_clear
  cache.io.debug_valid := io.debug_valid || io.onInit

  // 32KB
  val mem = SyncReadMem(1024, UInt(256.W))
  when(io.onInit) {
    mem.write(io.debug_clear_mem_addr, io.debug_clear_mem_data)
  }.elsewhen(cache.io.mem_req.write) {
    mem.write(cache.io.mem_req.waddr(12,4), cache.io.mem_req.wdata)
  }

  cache.io.mem_resp.valid := RegNext(cache.io.mem_req.read)//true.B
  cache.io.mem_resp.rdata := mem.read(cache.io.mem_req.raddr(12,4), cache.io.mem_req.read)




}