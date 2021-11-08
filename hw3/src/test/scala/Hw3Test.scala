import chisel3.iotesters.PeekPokeTester
import org.scalatest._
import Hw3._
import chisel3._
import chisel3.util._




class RegFileTest extends FlatSpec with Matchers {

  "RegFileTest" should "contain the stored value except for x0" in {
    val num_registers = 32
    chisel3.iotesters.Driver(() => new RegFile()) { dut => 
      new PeekPokeTester(dut) {
        val r = new scala.util.Random()
        r.setSeed(System.currentTimeMillis())
        val values = List.range(0,32).map((i) => {
          val vv = (BigInt(r.nextLong()))
          val v = if (vv < 0) -vv else vv
          val vstr = s"x${v.toString(16)}"
          poke(dut.io.rd, i.U(5.W))
          poke(dut.io.wdata, vstr.U(64.W))
          poke(dut.io.write, true.B)
          step(1)
          v
        })
        //println(values.mkString(", "))

        poke(dut.io.write, false.B)
        List.range(0,32).map((i) => {
          poke(dut.io.rs1, i.U)
          if (i == 0) {
            expect(dut.io.rs1_out, 0.U)
          } else {
            expect(dut.io.rs1_out, values(i).U)
          }
        })
        

        List.range(0,32).map((i) => {
          poke(dut.io.rs2, i.U)
          if (i == 0) {
            expect(dut.io.rs2_out, 0.U)
          } else {
            expect(dut.io.rs2_out, values(i).U)
          }
        })

        /* write values */

      }} should be (true)

  }

}

