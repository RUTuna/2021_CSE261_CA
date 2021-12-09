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
  cache_write := false.B
  windex := 0.U
  wtag := 0.U
  wvalid := false.B
  wdata := 0.U



  // idle state
  when(io.req.valid){
    req_reg := io.req
  }
  io.req_ready := Mux(io.req.valid, true.B, false.B)
  rindex := Mux(req_reg.valid, req_reg.addr(5,4), 0.U)
  
  val cdata_reg = RegInit(0.U(256.W))
  val rdata_reg = RegInit(0.U(64.W))



  // compare state
  val hit = Wire(Bool())
  val wb = RegInit(false.B)
  wb := Mux(fromValidArray && req_reg.write, true.B, false.B)
  hit := Mux(!wb && fromValidArray && fromTagArray === req_reg.addr(63,6), true.B, false.B)

  tag_reg := fromTagArray
  data_reg := fromDataArray


  
  // write back state
  io.mem_req.waddr := Cat(tag_reg, req_reg.addr(5,0))
  io.mem_req.write := state === s_write_back
  io.mem_req.wdata := data_reg



  // allocate state
  io.mem_req.raddr := req_reg.addr
  // Cat(req_reg.addr(63,4), 0.U(4.W))
  io.mem_req.read := state === s_allocate



  // wait state
  when(state === s_wait && io.mem_resp.valid){
    // rdata_reg := io.mem_resp.rdata(255,192)
    cache_write := state === s_wait
    windex := req_reg.addr(5,4)
    wtag := req_reg.addr(63,6)
    switch(req_reg.addr(3,0)){
      is(0.U) {
        rdata_reg := io.mem_resp.rdata(63,0)
      }

      is(4.U){
        rdata_reg := io.mem_resp.rdata(127, 64)
      }

      is(8.U){
        rdata_reg := io.mem_resp.rdata(191,128)
      }

      is(12.U){
        rdata_reg := io.mem_resp.rdata(255,192)
      }
    }


    // wdata := io.mem_resp.rdata
    when(req_reg.write){
    // when(req_reg.write){
      switch(req_reg.addr(3,0)){
        is(0.U) {
          wdata := Cat(io.mem_resp.rdata(255,64), req_reg.wdata(63,0))
        }

        is(4.U){
          val front = Cat(io.mem_resp.rdata(255,128), req_reg.wdata(63,0))
          wdata := Cat(front, io.mem_resp.rdata(63,0))
        }

        is(8.U){
          val front = Cat(io.mem_resp.rdata(255,192), req_reg.wdata(63,0))
          wdata := Cat(front, io.mem_resp.rdata(127,0))
        }

        is(12.U){
          wdata := Cat(req_reg.wdata(63,0), io.mem_resp.rdata(191,0))
        }
      }
    } .otherwise {
      wdata := io.mem_resp.rdata
    }
    cdata_reg := wdata
    printf("we cahed %d at %x\n", req_reg.addr(3,0), wdata)
    wvalid := true.B
  }


  // resp gen
  io.resp.valid := Mux(hit && state === s_compare_tag || state === s_wait && fromValidArray && fromDataArray === cdata_reg && wdata === 0.U, true.B, false.B)
  when(hit && state === s_compare_tag){
    io.resp.rdata := fromDataArray
  } .elsewhen( state === s_wait ){
    io.resp.rdata := rdata_reg
  } .otherwise{
    io.resp.rdata := 0.U
  }
  


  switch(state){
    is(s_idle){
      state := Mux(io.req.valid, s_compare_tag, s_idle)
    }

    is(s_compare_tag){
      io.req_ready := Mux(hit, true.B, false.B)
      when(hit) {
        state := s_idle
      } .elsewhen(wb) {
        state := s_write_back
      } .otherwise {
        state := s_allocate
      }
    }

    is(s_allocate){
      // state := Mux(wb, Mux(io.mem_resp.rdata === data_reg, s_wait, s_allocate), s_wait)
      state := s_wait
    }

    is(s_wait){
      val isChached = Wire(Bool())
      isChached := Mux(fromValidArray && fromDataArray === cdata_reg && wdata === 0.U, true.B, false.B)
      state := Mux(io.req.valid, s_compare_tag, Mux(isChached, s_idle, s_wait))
      io.req_ready := Mux(isChached, true.B, false.B)
    }

    is(s_write_back){
      state := s_allocate
    }
  }
  
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
    printf("cache_write %d %d %x %d\n",windex ,wtag ,wdata, wvalid)
    tagArray.write(windex, wtag)
    dataArray.write(windex, wdata)
    validArray.write(windex, wvalid)
  }
  


  // Logging

  val cycle = RegInit(0.U(64.W))
  cycle := Mux(io.debug_valid, 0.U, cycle + 1.U)
  when(!io.debug_valid) {
    printf(p"$cycle: (state: $state)\n")
    printf(p"$req_reg\n")
    // printf("array %d %d %x\n", fromTagArray, fromValidArray, fromDataArray)
    printf("wdata %x %x %x\n", wdata, cdata_reg, rdata_reg)
    printf(p"$io\n")
  }
  
  
  


}


