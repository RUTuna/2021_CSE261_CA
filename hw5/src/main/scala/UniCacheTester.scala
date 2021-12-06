package UniCache

import chisel3._
import chisel3.util._
import chisel3.iotesters.PeekPokeTester
import firrtl_interpreter._
import scala.collection.mutable.ListBuffer
import scala.sys.process._
import scala.util.control._
import scala.collection.mutable.Queue
// import FileWriter and BufferedWriter
import java.io.FileWriter
import java.io.BufferedWriter
import java.io.File

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




case class IdLog()
case class ExLog()
case class MemLog()
case class WBLog()
case class PipeLog(id: IdLog, ex: ExLog, mem: MemLog, wb: WBLog)

case class LogEntry(cycle: Int,
pc: BigInt, insn: BigInt, write: BigInt,
addr: BigInt,
wdata: BigInt, read: BigInt, rdata: BigInt, halted: BigInt) {
  override def toString() = {
    s"[${cycle}] pc: ${pc.toString(16)}, insn: ${insn.toString(16)}, write: ${write}, wdata: ${wdata.toString(16)}, addr: ${addr.toString(16)}, read: ${read}, rdata: ${rdata.toString(16)}, halted: ${halted}"
  }
}


case class Request(valid: String, write: String, addr: String, wdata: String) {
  override def toString() = {
    s"valid: ${valid.U.litValue()}, write: ${write.U.litValue()}, addr: ${addr.U.litValue().toString(16)}, wdata: ${wdata.U.litValue().toString(16)}"
  }
}

class UniCacheTester(trace: Queue[Request]) {

  val firrtl = (new chisel3.stage.ChiselStage).emitFirrtl(new UniCacheBase())

  def dumpCache(tester: InterpretiveTester): String = {
    val header = f"${" Tag "} | ${"Valid"} | ${"Data (Slot3)"}%16s | ${"Data (Slot2)"}%16s | ${"Data (Slot1)"}%16s | ${"Data (Slot0)"}%16s\n"
    header ++ (0 to 3).toList.map(i => {
      val tag = tester.peekMemory("cache.tagArray",i).toString(16)
      val data = tester.peekMemory("cache.dataArray",i)
      val dataArray = (0 to 3).toList.map(j => {
        val base = (data >> (j*64))
        val b = (data >> (j*64)) & ((BigInt(1) << 64) - 1)
        f"${"%016X".format(b)}%16s"
      }).reverse
      val valid = tester.peekMemory("cache.validArray",i).toString(16)
      val line = f"$tag%5s | [ $valid%1s ] | ${dataArray.mkString(" | ")}"
      line
    }).mkString("\n")
  }

  val requests = trace
  def run(): List[(String,String)] = {
    val states = ListBuffer[(String, String)]()
    val tester = new InterpretiveTester(firrtl)
    tester.poke("io_req_valid", 0)
    tester.poke("reset", 0)
    tester.poke("reset", 1)
    tester.poke("io_onInit", 1)
    //tester.poke("reset", 0)
    tester.step(1)
    tester.poke("reset", 0)
    //val log: ListBuffer[LogEntry] = ListBuffer()
    val loop = new Breaks
    //println(s"Simulation Started for ${progSourceName}")
    
    
    for (i <- 0 until 1024) {
      val bi = BigInt(i + 0xFFF0000)
      tester.poke("io_debug_clear_mem_addr", i )
      tester.poke("io_debug_clear_mem_data", bi | bi << 64 | bi << 128 | bi << 192)
      tester.step(1)
    }
    tester.poke("io_onInit", 0)
    for (i <- 0 until 4) {
      tester.poke("io_debug_valid", 1)
      tester.poke("io_debug_clear", i)
      tester.step(1)
    }
    tester.poke("io_debug_valid", 0)
    
    
    states.append(("",dumpCache(tester)))
    var onFly = false
    loop.breakable {
      for (i <- 0 until 300) {
        tester.poke("io_req_valid", 0)
        if (onFly) {
          if (tester.peek("io_resp_valid") == 1) {
            val resp = tester.peek("io_resp_rdata")
            states.append((resp.toString(16),dumpCache(tester)))
            onFly = false
          }
        }
        if (!onFly) {   
          if(requests.isEmpty) {
            loop.break
          } else {
            val thisRequest = requests.dequeue()
            tester.poke("io_req_valid", thisRequest.valid.U.litValue())
            tester.poke("io_req_addr", thisRequest.addr.U.litValue())
            tester.poke("io_req_wdata", thisRequest.wdata.U.litValue())
            tester.poke("io_req_write", thisRequest.write.U.litValue())
            onFly = true
          }       
        }
        
        tester.step(1)
      }
    }
    states.toList
  }
}

/**
* An object extending App to generate the Verilog code.
*/
object Main extends App with Helper {

  val tests = if(args.length > 0) {
    Hw5Tests.tests.filter(t => t._1 == args(0))
  } else {
    Hw5Tests.tests
  }
  val res = tests.map(t => {
    var log = ""
    log ++= ("-" * 80) + "\n"
    log ++= s"Running Test: ${t._1}" + "\n"
    log ++= s"Messages from the Simulator:" + "\n"
    val thisLogTup = (new UniCacheTester(t._2.clone())).run()
    val thisLog = thisLogTup.map(_._2)
    val resps = thisLogTup.map(_._1)
    log ++= s"Finished simulation for ${t._1}" + "\n"
    println(log)
    log = ""
    log ++= s"cache state changes:" + "\n"
    log ++= s"initially:" + "\n"
    log ++= s"${thisLog(0)}" + "\n"
    (1 until thisLog.length).toList.foreach(i => {
      log ++= s"after ${t._2.toList(i-1)}: "
      log ++= {
        if (t._2.toList(i-1).write.U.litValue() == 0) {
          s"(resp: ${resps(i-1)})" + "\n"
        } else {
          "\n"
        }
      }
      log ++= s"${thisLog(i)}" + "\n"
    })
    log ++= "-" * 80 + "\n"
    val check = "\n" + log == t._3
    
    //println(log)
    //write log to a file
    val file = new File(s"${t._1}.log")
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(log)
    bw.close()
    println(s"saved the cache state changes to file ${t._1}.log")
    if (check) {
      s"Test ${t._1} passed"
    } else {
      s"Test ${t._1} failed" ++ "\n" ++
      s"Expected:" ++ "\n" ++
      t._3 ++ "\n" ++
      s"Got:" ++ "\n" ++
      log
    }
  })
  println(s"selected tests: ${tests.map(_._1).mkString(", ")}")
  println(res.mkString("\n"))

}
