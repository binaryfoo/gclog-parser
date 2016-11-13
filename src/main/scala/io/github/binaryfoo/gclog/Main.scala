package io.github.binaryfoo.gclog

import java.io.{BufferedReader, File, FileReader}

import io.github.binaryfoo.gclog.output._
import scopt.OptionParser

/**
  * Convert a GC log to a tab, comma, pipe (or whatever) separated file.
  */
object Main {

  def main(args: Array[String]): Unit = {
    inputParser().parse(args, CmdLineOptions()).foreach { options =>
      val readers = options.inputs match {
        case Nil => Seq(Console.in)
        case files => files.map(f => new BufferedReader(new FileReader(f)))
      }
      val sink = options.sink
      for (reader <- readers) {
        // could be smarter than reading all to memory...
        val input = StdIn.readAllInput(reader)
        val baseEvents = if (options.heapStats)
          Parser.parseWithHeapStats(input)
        else
          Parser.parseLog(input)
        val collectionEvents = baseEvents.filterNot(_.gcType == AppPausedEvent.GcType)
        val eventsWithRates = new RateCalculator().apply(collectionEvents)
        val events = options.limit.map(n => eventsWithRates.take(n)).getOrElse(eventsWithRates)
        options.format.write(events, sink)
      }
      sink.close()
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

      opt[String]("dest") action { (destination, c) =>
        c.copy(sink = SocketSink(destination))
      } text "host:port to receive the events"

      opt[Int]('n', "limit") action { (limit, c) =>
        c.copy(limit = Some(limit))
      } text "Output at most N events"

      arg[File]("<gc-log-file>") action { (file, c) =>
        c.copy(inputs = c.inputs :+ file)
      } text "GC log files to read. Defaults to standard input"
    }
  }
}

case class CmdLineOptions(inputs: Seq[File] = Seq.empty,
                          heapStats: Boolean = false,
                          format: OutputFormat = TsvOutputFormat(),
                          sink: OutputSink = StdOutSink,
                          limit: Option[Int] = None)
