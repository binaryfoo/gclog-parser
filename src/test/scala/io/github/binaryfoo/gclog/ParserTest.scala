package io.github.binaryfoo.gclog

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
}
