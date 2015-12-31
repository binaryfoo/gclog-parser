package io.github.binaryfoo.gclog

object ToTsv {

  def main(args: Array[String]): Unit = {
    val input = StdIn.readAllInput()
    val events = Parser.parseLog(input)

    val w = Console.out
    val delimiter = "\t"
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
