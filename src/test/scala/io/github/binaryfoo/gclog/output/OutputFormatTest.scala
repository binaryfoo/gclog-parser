package io.github.binaryfoo.gclog.output

import java.io.{ByteArrayOutputStream, PrintStream}

import io.github.binaryfoo.gclog.{BasicGCEvent, GcLogTest, Parser}

class OutputFormatTest extends GcLogTest {

  "Graphite format" should "exclude datetime" in {
    val events = Parser.parseLog[BasicGCEvent](testInput("fragment.txt"))
    val baos = new ByteArrayOutputStream()
    val sink = PrintStreamSink(new PrintStream(baos))
    GraphiteOutputFormat().write(events.take(1), sink)
    baos.toString shouldBe """gc.age 524176.359 1449722814
                             |gc.type GC 1449722814
                             |gc.pause 0.18402 1449722814
                             |gc.heapBefore 3748762624 1449722814
                             |gc.heapAfter 2875555840 1449722814
                             |gc.heapReclaimed 873206784 1449722814
                             |gc.heapMax 4046913536 1449722814
                             |gc.PSYoungGenBefore 1096065024 1449722814
                             |gc.PSYoungGenAfter 78150656 1449722814
                             |gc.PSYoungGenReclaimed 1017914368 1449722814
                             |gc.PSYoungGenMax 1183580160 1449722814
                             |gc.promoted 144707584 1449722814
                             |gc.desiredSurvivorSize 248053760 1449722814
                             |gc.newThreshold 1 1449722814
                             |""".stripMargin
  }
}
