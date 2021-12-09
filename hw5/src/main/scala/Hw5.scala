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

  rindex := 0.U
  cache_write := false.B
  windex := 0.U
  wtag := 0.U
  wvalid := false.B
  wdata := 0.U
  io.mem_req.waddr := 0.U
  io.mem_req.write := false.B
  io.mem_req.wdata := 0.U
  io.mem_req.raddr := 0.U
  io.mem_req.read := 0.U

  io.resp.valid := false.B
  io.resp.rdata := 0.U
  io.req_ready := false.B
  
  switch(state){
    is(s_idle){
      // idle state
      // printf("s_idle\n")
      // printf("req valid %d\n", io.req.valid)

      when(io.req.valid){
        req_reg := io.req
        rindex := req_reg.addr(5,4)
        state := s_compare_tag
        // printf(p"i'm in! $req_reg\n")
      }
    }

    is(s_compare_tag){
      // compare state
      // printf("s_compare_tag\n")
      when(fromValidArray && fromTagArray === io.req.addr(63,6)){ // HIT
        when(io.req.write){ // write
          printf(p"Hit write $state\n")
          cache_write := true.B
          windex := io.req.addr(5,4)
          wtag := fromTagArray
          wdata := io.req.wdata
          wvalid := true.B
        } .otherwise { // read
          printf(p"Hit read $state\n")
          cache_write := false.B
          io.resp.rdata := fromDataArray
        }
        io.req_ready := true.B
        io.resp.valid := true.B
        state := s_idle // go idle
      } .elsewhen(fromValidArray) { // MISS 누가 사용 중
        printf(p"Miss wb $state\n")
        cache_write := false.B
        tag_reg := fromTagArray // origin block tag 저장
        data_reg := fromDataArray // origin block data 저장
        state := s_write_back
      } .otherwise { // MISS 새로 추가해줘야함
        printf(p"Miss allo $state\n")
        cache_write := false.B
        state := s_allocate
      }
    }

    is(s_allocate){
      // printf("s_allocate\n")
      io.mem_req.raddr := Cat(io.req.addr(63,4), 0.U(4.W))
      io.mem_req.read := true.B
      state := s_wait
    }

    is(s_wait){
      // printf("s_wait\n")
      io.req_ready := false.B
      when(io.mem_resp.valid){
        cache_write := true.B
        rindex := io.req.addr(5,4)
        windex := io.req.addr(5,4)
        wtag := io.req.addr(63,6)
        wvalid := true.B

        when(io.req.write){
          switch(io.req.addr(3,0)){
            is(0.U) {
              wdata := Cat(io.mem_resp.rdata(255,64), io.req.wdata(63,0))
            }

            is(4.U){
              val front = Cat(io.mem_resp.rdata(255,128), io.req.wdata(63,0))
              wdata := Cat(front, io.mem_resp.rdata(63,0))
            }

            is(8.U){
              val front = Cat(io.mem_resp.rdata(255,192), io.req.wdata(63,0))
              wdata := Cat(front, io.mem_resp.rdata(127,0))
            }

            is(16.U){
              wdata := Cat(io.req.wdata(63,0), io.mem_resp.rdata(191,0))
            }
          }
        } .otherwise {
          wdata := io.mem_resp.rdata
        }
      }

      when(fromValidArray){
        io.resp.rdata := fromDataArray
        io.resp.valid := true.B
        io.req_ready := true.B
        state := s_idle
      } 
    }

    is(s_write_back){
      // printf("s_write_back\n")
      io.mem_req.waddr := io.req.addr
      io.mem_req.write := true.B

      switch(io.req.addr(3,0)){
        is(0.U) {
          io.mem_req.wdata := Cat(data_reg(255,64), io.req.wdata(63,0))
        }

        is(4.U){
          val front = Cat(data_reg(255,128), io.req.wdata(63,0))
          io.mem_req.wdata := Cat(front, data_reg(63,0))
        }

        is(8.U){
          val front = Cat(data_reg(255,192), io.req.wdata(63,0))
          io.mem_req.wdata := Cat(front, data_reg(127,0))
        }

        is(16.U){
          io.mem_req.wdata := Cat(io.req.wdata(63,0), data_reg(191,0))
        }
      }
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
    // printf(p"cache_write $windex $wtag $wdata $wvalid\n")
    tagArray.write(windex, wtag)
    dataArray.write(windex, wdata)
    validArray.write(windex, wvalid)
  }
  


  // Logging

  val cycle = RegInit(0.U(64.W))
  cycle := Mux(io.debug_valid, 0.U, cycle + 1.U)
  when(!io.debug_valid) {
    printf(p"$cycle: (state: $state)\n")
    // printf(p"$req_reg $tag_reg $data_reg $valid_reg\n")
    // printf(p"array $fromTagArray $fromValidArray $fromDataArray $rindex\n")
    // printf(p"$io\n")
  }
  
  
  


}


