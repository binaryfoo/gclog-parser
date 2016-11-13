package io.github.binaryfoo.gclog.output

import io.github.binaryfoo.gclog.GCEvent

sealed trait OutputFormat {
  def write(events: Seq[GCEvent], w: OutputSink)
}

case class TsvOutputFormat(delimiter: String = "\t") extends OutputFormat {

  override def write(events: Seq[GCEvent], w: OutputSink): Unit = {
    events.headOption.foreach { e =>
      val header = e.toSeq.map(_._1).mkString(delimiter)
      w.write(header)
    }
    events.foreach { e =>
      val line = e.toSeq.map(_._2).mkString(delimiter)
      w.write(line)
    }
  }

}

case class GraphiteOutputFormat(prefix: String = "gc") extends OutputFormat {
  override def write(events: Seq[GCEvent], w: OutputSink): Unit = {
    for {
      event <- events
      timestamp = event.time.getMillis / 1000
      (metricName, value) <- event.toSeq if metricName != "datetime"
    } {
      // line protocol: <metric path> <metric value> <metric timestamp>
      // <metric timestamp> is unix epoch time (seconds since 1970)
      w.write(s"$prefix.$metricName $value $timestamp")
    }
  }
}
