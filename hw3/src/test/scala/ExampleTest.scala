import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import Hw3._
import chisel3._
import chisel3.util._
import scopt.Read

class IncrementingCounterTest extends FlatSpec with Matchers {

  "IncrementingCounter" should "pass" in {
    chisel3.iotesters.Driver(() => new IncrementingCounter()) { c =>
      new PeekPokeTester(c) {
        poke(c.io.inc, 0)
        poke(c.io.inc, 1)
        poke(c.io.reset, 1)
        step(1)
        expect(c.io.output, 0)
        println("1: " ++ peek(c.io.output).toString(16))
        step(1)
        poke(c.io.inc, 0)
        expect(c.io.output, 0)
        println("2: " ++ peek(c.io.output).toString(16))
        poke(c.io.inc, 1)
        poke(c.io.reset, 0)
        step(1)
        expect(c.io.output, 1)
        println("3: " ++ peek(c.io.output).toString(16))
        step(1)
        expect(c.io.output, 2)
        println("4: " ++ peek(c.io.output).toString(16))
        poke(c.io.inc, 0)
        step(1)
        expect(c.io.output, 2)
        println("5: " ++ peek(c.io.output).toString(16))
        poke(c.io.inc, 0)
        poke(c.io.reset, 1)
        step(1)
        expect(c.io.output, 0)
        println("6: " ++ peek(c.io.output).toString(16))
        
      }
    } should be (true)
  }

}


class RegisterModuleTest extends FlatSpec with Matchers {

  "RegisterModule" should "pass" in {
    chisel3.iotesters.Driver(() => new RegisterModule()) { c =>
      new PeekPokeTester(c) {
        poke(c.io.in, 0)
        step(1)
        expect(c.io.output, 1)
        println("1: " ++ peek(c.io.output).toString(16))
        step(1)
        expect(c.io.output, 1)
        println("2: " ++ peek(c.io.output).toString(16))

        poke(c.io.in, 5)
        step(1)
        expect(c.io.output, 6)
        println("3: " ++ peek(c.io.output).toString(16))
        step(1)
        expect(c.io.output, 6)
        println("4: " ++ peek(c.io.output).toString(16))
        poke(c.io.in, 0)
        step(1)
        expect(c.io.output, 1)
        println("5: " ++ peek(c.io.output).toString(16))
        poke(c.io.in, 0)
        step(1)
        expect(c.io.output, 1)
        println("6: " ++ peek(c.io.output).toString(16))
        
      }
    } should be (true)
  }

}