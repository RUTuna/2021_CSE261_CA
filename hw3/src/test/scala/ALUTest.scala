import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import Hw3._
import chisel3._
import chisel3.util._

class ALUTest extends FlatSpec with Matchers with Helper {

  "ALU" should "compute" in {
    chisel3.iotesters.Driver( () => new ALU()) { dut =>
      new PeekPokeTester(dut) {
        val tests = List(
          Array("b0000", "b101", "b011", "b1")
          ,Array("b0001", "b101", "b011", "b111")
          ,Array("b0010", "b1", "b1", "x2")
          ,Array("b0110", "x12", "x01", "x11")
          ,Array("b0110", "x11", "x01", "x10")
        ).map(e => e.map(_.U))
        tests.foreach((t) => {
          poke(dut.io.ctrl, t(0))
          poke(dut.io.a, t(1))
          poke(dut.io.b, t(2))
          expect(dut.io.res, t(3))
        })

      }
    } should be (true)
  }
}



class RegExample extends Module {
  val io = IO(new Bundle {
    val D = Input(UInt(1.W))	
    val Q = Output(UInt(1.W))
  })
  val r = Reg(UInt(1.W))
  r := io.D
  io.Q := r
}

class RegEnableExample extends Module { 			// This is a module
  val io = IO(new Bundle {				// Start of inputs
    val D = Input(UInt(1.W))			// first input
    val Write = Input(Bool())
    val Q = Output(UInt(1.W))			// output
  })
  val r = RegEnable(UInt(1.W), enable = io.Write)
  io.Q := r
}


/**
* An object extending App to generate the Verilog code.
*/
/*object Hello extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Hello())
}*/



/**
* Task 2: Simple Counter
*/



class IncrementingCounter extends Module {
  val io = IO(new Bundle {
    val inc = Input(Bool())
    val reset = Input(Bool())
    val output = Output(UInt(32.W))
  })
  
  val counter = RegInit(0.U(32.W))
  
  counter := Mux(io.reset, 0.U(32.W), Mux (
  io.inc, counter + 1.U(32.W), counter
  ))
  io.output := counter
  /* your code to here */
  
  
}
