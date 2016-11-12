package io.github.binaryfoo.gclog

import java.io.PrintStream

import scopt.OptionParser

/**
  * Convert a GC log to a tab, comma, pipe (or whatever) separated file.
  */
object Main {

  def main(args: Array[String]): Unit = {
    inputParser().parse(args, CmdLineOptions()).foreach { options =>
      val input = StdIn.readAllInput()
      val baseEvents = if (options.heapStats)
        Parser.parseWithHeapStats(input)
      else
        Parser.parseLog(input)
      val eventsWithRates = new RateCalculator().apply(baseEvents)
      val events = options.limit.map(n => eventsWithRates.take(n)).getOrElse(eventsWithRates)

      val w = Console.out
      options.format.write(events, w)
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

      opt[String]('o', "format") action { (format, c) =>
        val outputFormat = format match {
          case "tsv" => TsvOutputFormat()
          case "graphite" => GraphiteOutputFormat()
        }
        c.copy(format = outputFormat)
      } text "Output format. tsv or graphite. Defaults to tsv"

      opt[String]("delimiter") action { (delimiter, c) =>
        c.copy(format = TsvOutputFormat(delimiter))
      } text "Field delimiter. Defaults to tab (\\t)"

      opt[String]("prefix") action { (prefix, c) =>
        c.copy(format = GraphiteOutputFormat(prefix))
      } text "Metric path prefix for graphite output"

      opt[Int]('n', "limit") action { (limit, c) =>
        c.copy(limit = Some(limit))
      } text "Output at most N events"
    }
  }
}

sealed trait OutputFormat {
  def write(events: Seq[GCEventWithRates], w: PrintStream)
}

case class TsvOutputFormat(delimiter: String = "\t") extends OutputFormat {

  override def write(events: Seq[GCEventWithRates], w: PrintStream): Unit = {
    events.headOption.foreach { e =>
      val header = e.toSeq.map(_._1).mkString(delimiter)
      w.println(header)
    }
    events.foreach { e =>
      val line = e.toSeq.map(_._2).mkString(delimiter)
      w.println(line)
    }
  }

}

case class GraphiteOutputFormat(prefix: String = "gc") extends OutputFormat {
  override def write(events: Seq[GCEventWithRates], w: PrintStream): Unit = {
    for {
      event <- events
      timestamp = event.time.getMillis / 1000
      (metricName, value) <- event.toSeq if metricName != "dateTime"
    } {
      // line protocol: <metric path> <metric value> <metric timestamp>
      // <metric timestamp> is unix epoch time (seconds since 1970)
      w.println(s"$prefix.$metricName $value $timestamp")
    }
  }
}

case class CmdLineOptions(heapStats: Boolean = false, format: OutputFormat = TsvOutputFormat(), limit: Option[Int] = None)
