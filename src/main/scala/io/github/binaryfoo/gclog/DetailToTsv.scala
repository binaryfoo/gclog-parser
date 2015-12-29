package io.github.binaryfoo.gclog

import io.github.binaryfoo.gclog.SuffixExpander.toBytes

object DetailToTsv {

  def main(args: Array[String]): Unit = {
    val input = StdIn.readAllInput()
    val events = Parser.parseWithHeapStats(input)

    val regionNames = events.head.regions.map(_.name).flatMap(name => Seq(name + "Before", name + "After"))
    val header = Seq("time", "pause", "heapBefore", "heapAfter", "heapMax", "type") ++ regionNames

    val w = Console.out
    w.println(header.mkString("\t"))
    events.foreach { detail =>
      val e = detail.e
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
      ) ++ detail.regions.flatMap(r => Seq(toBytes(r.start), toBytes(r.end)))
      w.println(line.mkString("\t"))
    }
    w.flush()
  }

}
