package io.github.binaryfoo.gclog

import java.io.File
import java.nio.file.Files

import org.scalatest.{Matchers, FlatSpec}

class GcLogTest extends FlatSpec with Matchers {

  def testInput(fileName: String): String = {
    new String(Files.readAllBytes(new File(s"src/test/resources/$fileName").toPath))
  }

}
