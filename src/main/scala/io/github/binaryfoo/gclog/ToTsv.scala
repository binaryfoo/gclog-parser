package io.github.binaryfoo.gclog

import scala.collection.mutable

import SuffixExpander.toBytes

object ToTsv {

  def main(args: Array[String]): Unit = {
    val input = readAllInput()
    val events = Parser.parseLog(input)

    val w = Console.out
    w.println("time\tpause\theapBefore\theapAfter\theapMax\ttype")
    events.foreach { e =>
      val line = Seq(
        e.time.toString("yyyy-MM-dd HH:mm:ss.SSS"),
        e.pauseSeconds,
        toBytes(e.heapDelta.start),
        toBytes(e.heapDelta.end),
        toBytes(e.heapDelta.capacity),
        e.gcType
      )
      w.println(line.mkString("\t"))
    }
    w.flush()
  }

  private def readAllInput(): String = {
    val buffer = mutable.ArrayBuffer[String]()
    while (true) {
      val line = Console.in.readLine()
      if (line == null) {
        return buffer.mkString("\n")
      }
      buffer += line
    }
    buffer.mkString("\n")
  }
}
