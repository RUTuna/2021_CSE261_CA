package Hw3

import chisel3._
import chisel3.util._
import chisel3.iotesters.PeekPokeTester
import firrtl_interpreter._
import scala.collection.mutable.ListBuffer
import scala.sys.process._
import scala.util.control._

trait Helper {
  def pat(b: BigInt, p: BitPat): Boolean = {
    (b & p.mask) == p.value
  }
  
  def loadHexDump(fname: String): (String, Array[String]) = {
    val hex = io.Source.fromFile(fname).mkString.split('\n').drop(2)
    val base = hex(0).split("\\s+")(0).drop(2)
    val bytes = hex.map((l) => {
      val bb = l.slice(0,49).split("\\s+").drop(2)
      //println(bb.mkString(", "))
      bb.map(a => Array(a.slice(0,2), a.slice(2,4), a.slice(4,6), a.slice(6,8))).flatten
    }).flatten
    
    (base,bytes.map("x" + _))
  }
}







case class LogEntry(cycle: Int,
pc: BigInt, insn: BigInt, write: BigInt,
addr: BigInt,
wdata: BigInt, read: BigInt, rdata: BigInt, halted: BigInt) {
  override def toString() = {
    s"[${cycle}] pc: ${pc.toString(16)}, insn: ${insn.toString(16)}, write: ${write}, wdata: ${wdata.toString(16)}, addr: ${addr}, read: ${read}, rdata: ${rdata.toString(16)}, halted: ${halted}"
  }
}


class TestHelper(progSourceName: String) extends Helper {
  if(!progSourceName.endsWith(".s")) {
    throw new Exception("SourceName must end with .s")
  }
  val execBinName = progSourceName.dropRight(2) + ".elf"
  val execHexName = progSourceName.dropRight(2) + ".hex"
  val execDumpName = progSourceName.dropRight(2) + ".d"
  val riscvBinPath = "../tools/riscv/bin/"
  val compilerPath = if(new java.io.File(riscvBinPath).exists) {
    riscvBinPath
  } else {
    "tools/riscv/bin/"  
  }
  val compiler = compilerPath ++ "riscv64-unknown-elf-gcc"
  val readelf = compilerPath ++ "riscv64-unknown-elf-readelf"
  val objdump = compilerPath ++ "riscv64-unknown-elf-objdump"
  val flags = "-nostdlib -nostartfiles -march=rv64i -mabi=lp64 -Wl,--section-start=.text=0x0 -o"
  //val toHexCmd = s"$readelf -R .text -x .rodata -x .data"
  val toHexCmd = s"$readelf -R .text"
  val getObjDump = s"$objdump -d"
  
  def compile(): Boolean = {
    val cmd = s"$compiler $flags $execBinName $progSourceName"
    val stdout = new StringBuilder
    val stderr = new StringBuilder
    val res = cmd ! ProcessLogger(stdout append _, stderr append _)
    if (res != 0) {
      print(stdout)
      print(stderr)
      throw new Exception(s"Failed to compile ${progSourceName}")
    }
    val toHex = s"$toHexCmd $execBinName"
    val stdout2 = new StringBuilder
    val stderr2 = new StringBuilder
    val res2 = toHex.!!// ProcessLogger(stdout2 append _, stderr2 append _)
    // write res2 to file execHexName
    val f = new java.io.File(execHexName)
    val pw = new java.io.PrintWriter(f)
    //println(stdout2.mkString("\n"))
    //println(stderr2.mkString)
    pw.write(res2)
    pw.close()
    // obtain objdump and save to file
    val getObjDumpCmd = s"$getObjDump $execBinName"
    val stdout3 = new StringBuilder
    val stderr3 = new StringBuilder
    val res3 = getObjDumpCmd.!!// ProcessLogger(stdout3 append _, stderr3 append _)
    val f2 = new java.io.File(execDumpName)
    val pw2 = new java.io.PrintWriter(f2)
    //println(stdout3.mkString("\n"))
    //println(stderr3.mkString)
    pw2.write(res3)
    pw2.close()
    
    (res == 0)
  }
  
  
  val prog: Array[String] = {
    compile()
    loadHexDump(execHexName)._2
  }
  
  
  val firrtl = (new chisel3.stage.ChiselStage).emitFirrtl(new BaseSystem(prog))
  
  def run(): String = {
    val tester = new InterpretiveTester(firrtl)
    tester.poke("reset", 1)
    tester.step(1)
    tester.poke("reset", 0)
    val log: ListBuffer[LogEntry] = ListBuffer()
    val loop = new Breaks
    println(s"Simulation Started for ${progSourceName}")
    
    loop.breakable {
      for (i <- 0 until 30) {
        tester.step(1)
        log.append(LogEntry(i,
        tester.peek("io_trace_pc"), 
        tester.peek("io_trace_insn"),
        tester.peek("io_trace_write"),
        tester.peek("io_trace_addr"),
        tester.peek("io_trace_wdata"),
        tester.peek("io_trace_read"),
        tester.peek("io_trace_rdata"),
        tester.peek("io_trace_halted"),
        ))
        val halted = tester.peek("io_trace_halted")
        if(halted == 1) {
          //println("halted")
          loop.break
        }
      }
    }
    println(s"Finishied Simulation for ${progSourceName}")
    
    log.mkString("\n")
  }
}


/**
* An object extending App to generate the Verilog code.
*/
object Main extends App with Helper {
  
  if(args.length > 0) {
    val progSourceName = args(0)
    val testHelper = new TestHelper(progSourceName)
    val log = testHelper.run()
    println(log)
  } else {

  val resultLogs = Hw3Tests.instructionTests.map((t) => {
    
    val path = Hw3Tests.testHome ++ "/" ++ t._2
    val log = (new TestHelper(path)).run()
    val result = log.trim()
    val ref = t._3.trim()
    val checked = if ( result == ref) {
      "passed"
    } else {
      
      "failed" ++ "\nreference:\n" ++ ref ++ "\nresult:\n" ++ result
    }
    s"tested ${t._1} (${t._2}): ${checked}"
  })
  
    println("test results:")
    println(resultLogs.mkString("\n"))

  }
  
  
}
