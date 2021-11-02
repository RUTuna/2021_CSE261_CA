/*
* This code is a minimal hardware described in Chisel.
* 
* Blinking LED: the FPGA version of Hello World
* 
* Note
* https://mybinder.org/v2/gh/freechipsproject/chisel-bootcamp/master
*/

package Hw2
import chisel3._
import chisel3.util._
import dotvisualizer.stage.DiagrammerStage
import java.io.{ByteArrayOutputStream, File, PrintStream}
import chisel3.iotesters.PeekPokeTester






// Chisel Code: Declare a new module definition
class Passthrough extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(4.W))
    val out = Output(UInt(4.W))
  })
  io.out := io.in
  printf("out: %x\n", io.out)
}

class MyOperatorsTwo extends Module {
  val io = IO(new Bundle {
    val cond    = Input(Bool())
    val a      = Input(UInt(4.W))
    val b      = Input(UInt(4.W))
    val out_mux = Output(UInt(4.W))
    val out_cat = Output(UInt(8.W))
  })

  val s = true.B
  io.out_mux := Mux(io.cond, io.a, io.b) // should return 3.U, since s is true
  io.out_cat := Cat(io.a, io.b)    // concatenates 2 (b10) with 1 (b1) to give 5 (101)
}

class And extends Module {
  
  val io = IO(new Bundle {
    val a = Input(UInt(1.W))
    val b = Input(UInt(1.W))
    val y = Output(UInt(1.W))
  })
  
  io.y := io.a & io.b
}

object Main extends App {
  val verilog = stage.ChiselStage.emitVerilog(new Passthrough)
  println(verilog)
  
  val outputBuf = new ByteArrayOutputStream()
  Console.withOut(new PrintStream(outputBuf)) {
    (new DiagrammerStage).execute(
    Array("--target-dir", ".", "--rank-elements", "--open-command", ""),
    Seq(stage.ChiselGeneratorAnnotation(() => new Passthrough()))
    )
  }
  chisel3.iotesters.Driver(() => new Passthrough()) { c => 
    new PeekPokeTester(c) {
        poke(c.io.in, 0.U)     // Set our input to value 0
        expect(c.io.out, 0.U)  // Assert that the output correctly has 0
        poke(c.io.in, 1.U)     // Set our input to value 1
        expect(c.io.out, 1.U)  // Assert that the output correctly has 1
        poke(c.io.in, 2.U)     // Set our input to value 2
        expect(c.io.out, 2.U)  // Assert that the output correctly has 2
        val o = peek(c.io.out)
        println(s"SUCCESS!! (${o})") // Scala Code: if we get here, our tests passed!
      }
    }
  Console.withOut(new PrintStream(outputBuf)) {
    (new DiagrammerStage).execute(
    Array("--target-dir", ".", "--rank-elements", "--open-command", ""),
    Seq(stage.ChiselGeneratorAnnotation(() => new MyOperatorsTwo()))
    )
  }
}




