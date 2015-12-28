package io.github.binaryfoo.gclog

import java.io.File
import java.nio.file.Files

import fastparse.all._
import io.github.binaryfoo.gclog.Parser._
import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.{FlatSpec, Matchers}

class ParserTest extends FlatSpec with Matchers {

  private val Plus11 = DateTimeZone.forOffsetHours(11)

  "Full GC" should "be parsed" in {
    val line = """2015-12-04T16:07:12.422+1100: 6994.482: [Full GC [PSYoungGen: 14194K->0K(1376448K)] [ParOldGen: 2788303K->1802287K(2796224K)] 2802498K->1802287K(4172672K) [PSPermGen: 66560K->66131K(132736K)], 3.8232380 secs] [Times: user=10.81 sys=0.06, real=3.83 secs]"""

    val Parsed.Success(value, _) = gcLine.parse(line)
    value.time shouldBe new DateTime(2015, 12, 4, 16, 7, 12, 422, Plus11)
    value.jvmAgeSeconds shouldBe 6994.482
    value.gcType shouldBe "Full GC"
    value.heapDelta shouldBe SizeDelta("2802498K", "1802287K", "4172672K")
    value.pauseSeconds shouldBe 3.823238
  }

  "Timestamp" should "be parsed" in {
    val Parsed.Success(value, _) = Timestamp.parse("2015-12-04T16:07:12.422+1100")

    value shouldBe new DateTime(2015, 12, 4, 16, 7, 12, 422, Plus11)
  }

  "Size stats" should "be parsed" in {
    val Parsed.Success(value, _) = SizeStats.parse("14194K->0K(1376448K)")
    value shouldBe SizeDelta("14194K", "0K", "1376448K")
  }

  "Generation stats" should "be parsed" in {
    val Parsed.Success(value, _) = GenerationStats.parse("[PSYoungGen: 14194K->0K(1376448K)]")
    value.name shouldBe "PSYoungGen"
    value.delta shouldBe SizeDelta("14194K", "0K", "1376448K")
  }

  "Promotion failure" should "be parsed" in {
    val line = """2015-12-10T15:42:08.076+1100: 523890.136: [GC-- [PSYoungGen: 1275256K->1275256K(1275264K)] 4007798K->4071477K(4071488K), 0.3913740 secs] [Times: user=0.54 sys=0.00, real=0.39 secs]"""

    val Parsed.Success(value, _) = gcLine.parse(line)
    value.gcType shouldBe "GC--"
    value.pauseSeconds shouldBe 0.391374
  }

  "Multi-line young generation collection" should "be parsed" in {
    val lines = """2015-12-10T15:43:18.274+1100: 523960.334: [GC
      |Desired survivor size 129368064 bytes, new threshold 3 (max 15)
      | [PSYoungGen: 1220800K->88639K(1260480K)] 3440993K->2372792K(4056704K), 0.1104060 secs] [Times: user=0.24 sys=0.01, real=0.12 secs]
      |""".stripMargin

    val Parsed.Success(value, _) = gcLine.parse(lines)
    value.time shouldBe new DateTime(2015, 12, 10, 15, 43, 18, 274, Plus11)
    value.heapDelta shouldBe SizeDelta("3440993K", "2372792K", "4056704K")
    value.generationDeltas shouldBe Seq(GenerationDelta("PSYoungGen", SizeDelta("1220800K", "88639K", "1260480K")))
    value.pauseSeconds shouldBe 0.110406
  }

  "Multiple events" should "be parsed" in {
    val events = Parser.parseLog(testInput("fragment.txt"))
    events(0).time shouldBe new DateTime(2015, 12, 10, 15, 46, 54, 299, Plus11)
    events(0).gcType shouldBe "GC"

    events(1).time shouldBe new DateTime(2015, 12, 10, 15, 46, 54, 493, Plus11)
    events(1).gcType shouldBe "Full GC"
  }

  "GC cause" should "be parsed" in {
    val Parsed.Success(value, _) = gcLine.parse("0.235: [GC (Allocation Failure)  65536K->57255K(251392K), 0.0222615 secs]")
    value.gcCause shouldBe "Allocation Failure"
  }

  "GC cause with details" should "be parsed" in {
    val Parsed.Success(value, _) = gcLine.parse("2015-12-28T13:50:37.116-1000: 0.251: [GC (Allocation Failure) [PSYoungGen: 65536K->10736K(76288K)] 65536K->57253K(251392K), 0.0217970 secs] [Times: user=0.09 sys=0.06, real=0.02 secs]")
    value.gcCause shouldBe "Allocation Failure"
  }

  "Metaspace sizing" should "be parsed" in {
    val Parsed.Success(value, _) = gcLine.parse("2015-12-28T13:50:37.214-1000: 0.349: [Full GC (Ergonomics) [PSYoungGen: 10720K->0K(141824K)] [ParOldGen: 109101K->117471K(290816K)] 119821K->117471K(432640K), [Metaspace: 4082K->4082K(1056768K)], 0.2284721 secs] [Times: user=1.42 sys=0.02, real=0.23 secs]")
    value.generationDeltas.collectFirst { case d: GenerationDelta if d.name == "Metaspace" => d }.get shouldBe GenerationDelta("Metaspace", SizeDelta("4082K", "4082K", "1056768K"))
  }

  "Basic jdk7 log" should "be parsed" in {
    val events = Parser.parseLog(testInput("basic-java7-gc.log"))
    events.size shouldBe 7

    events(0).jvmAgeSeconds shouldBe 0.263
    events(0).pauseSeconds shouldBe 0.022292
    events(6).jvmAgeSeconds shouldBe 2.832
    events(6).pauseSeconds shouldBe 0.022377
  }

  "Detailed jdk7 log" should "be parsed" in {
    val events = Parser.parseLog(testInput("details-java7-gc.log"))
    events.size shouldBe 7
  }

  "Basic jdk8 log" should "be parsed" in {
    val events = Parser.parseLog(testInput("basic-java8-gc.log"))
    events.size shouldBe 7

    events(0).jvmAgeSeconds shouldBe 0.235
    events(0).pauseSeconds shouldBe 0.0222615
    events(6).jvmAgeSeconds shouldBe 2.232
    events(6).pauseSeconds shouldBe 0.0209706
  }

  "Detailed jdk8 log" should "be parsed" in {
    val events = Parser.parseLog(testInput("details-java8-gc.log"))
    events.size shouldBe 7
  }

  def testInput(fileName: String): String = {
    new Predef.String(Files.readAllBytes(new File(s"src/test/resources/$fileName").toPath))
  }

}
