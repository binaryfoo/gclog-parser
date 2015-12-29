package io.github.binaryfoo.gclog

import io.github.binaryfoo.gclog.SuffixExpander.toBytes

object ToTsv {

  def main(args: Array[String]): Unit = {
    val input = StdIn.readAllInput()
    val events = Parser.parseLog(input)

    val w = Console.out
    w.println("time\tpause\theapBefore\theapAfter\theapMax\ttype")
    events.foreach { e =>
      val time = if (e.time != null) {
        e.time.toString("yyyy-MM-dd HH:mm:ss.SSS")
      } else {
        e.jvmAgeSeconds.toString
      }
      val line = Seq(
        time,
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

}
