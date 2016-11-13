package io.github.binaryfoo.gclog

import java.io.BufferedReader

import scala.annotation.tailrec
import scala.collection.mutable

object StdIn {
  def readAllInput(in: BufferedReader): String = {
    readToEnd(in, mutable.ArrayBuffer[String]())
  }

  @tailrec
  private def readToEnd(in: BufferedReader, buffer: mutable.ArrayBuffer[String]): String = {
    val line = in.readLine()
    if (line == null) {
      buffer.mkString("\n")
    } else {
      buffer += line
      readToEnd(in, buffer)
    }
  }
}
