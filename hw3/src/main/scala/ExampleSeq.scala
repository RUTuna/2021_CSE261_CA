package Hw3

import chisel3._
import chisel3.util._

/**
 * Task 0: A Simple Counter
 */

class IncrementingCounter extends Module {
  val io = IO(new Bundle {
    val inc = Input(Bool())
    val output = Output(UInt(32.W))
    val reset = Input(Bool())
  })

  val counter = RegInit(0.U(32.W))
  counter := Mux(io.reset, 0.U, Mux(io.inc, counter + 1.U, counter))
  io.output := counter

}

class RegisterModule extends Module {
  val io = IO(new Bundle {
    val in  = Input(UInt(12.W))
    val output = Output(UInt(12.W))
  })
  
  val register = Reg(UInt(12.W))
  register := io.in + 1.U
  io.output := register
}



