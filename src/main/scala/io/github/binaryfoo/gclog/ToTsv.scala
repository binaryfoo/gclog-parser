package io.github.binaryfoo.gclog

import scopt.OptionParser

/**
  * Convert a GC log to a tab, comma, pipe (or whatever) separated file.
  */
object ToTsv {

  def main(args: Array[String]): Unit = {
    inputParser().parse(args, CmdLineOptions()).foreach { options =>
      val input = StdIn.readAllInput()
      val events = if (options.heapStats)
        Parser.parseWithHeapStats(input)
      else
        Parser.parseLog(input)

      val w = Console.out
      val delimiter = options.delimiter
      events.headOption.foreach { e =>
        val header = e.toSeq.map(_._1).mkString(delimiter)
        w.println(header)
      }
      events.foreach { e =>
        val line = e.toSeq.map(_._2).mkString(delimiter)
        w.println(line)
      }
      w.flush()
    }
  }

  private def inputParser(): OptionParser[CmdLineOptions] = {
    new scopt.OptionParser[CmdLineOptions]("gclog-parser") {
      head(s"gclog-parser", "0.0.1")

      help("help") text "Show usage"

      opt[Boolean]("heap-stats") action { (heapStats, c) =>
        c.copy(heapStats = heapStats)
      } text "Input includes heap stats from JVM -XX:+PrintHeapAtGC option"

      opt[String]("delimiter") action { (delimiter, c) =>
        c.copy(delimiter = delimiter)
      } text "Field delimiter. Defaults to tab (\\t)"
    }
  }
}

case class CmdLineOptions(heapStats: Boolean = false, delimiter: String = "\t")
