import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import chisel3._
import chisel3.util._
import Hw2._
class PassThroughTest extends FlatSpec with Matchers {
  "Passthrough" should "pass the values" in {
    chisel3.iotesters.Driver(() => new Passthrough()) { c => 
    new PeekPokeTester(c) {
        poke(c.io.in, 0.U)     // Set our input to value 0
        expect(c.io.out, 0.U)  // Assert that the output correctly has 0
        step(1)
        poke(c.io.in, 1.U)     // Set our input to value 1
        expect(c.io.out, 1.U)  // Assert that the output correctly has 1
        poke(c.io.in, 2.U)     // Set our input to value 2
        expect(c.io.out, 2.U)  // Assert that the output correctly has 2
        val o = peek(c.io.out)
        println(s"SUCCESS!! (${o})") // Scala Code: if we get here, our tests passed!
      }
    }
  }
}
