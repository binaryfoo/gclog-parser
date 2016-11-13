package io.github.binaryfoo.gclog.output

import java.io.{PrintStream, PrintWriter}
import java.net.Socket

trait OutputSink {
  def write(event: String)
  def close()
}

object StdOutSink extends PrintStreamSink(Console.out)

case class PrintStreamSink(out: PrintStream) extends OutputSink {
  override def write(event: String): Unit = out.println(event)
  override def close(): Unit = out.flush()
}

case class SocketSink(host: String = "localhost", port: Int = 2003) extends OutputSink {

  private val socket = new Socket(host, port)
  private val out = new PrintWriter(socket.getOutputStream)

  override def write(event: String): Unit = out.println(event)

  override def close(): Unit = {
    out.close()
    socket.close()
  }
}

object SocketSink {
  def apply(destination: String): SocketSink = {
    val Array(host, port) = destination.split(':')
    SocketSink(host, port.toInt)
  }
}
