package io.github.binaryfoo.gclog

import fastparse.all._
import io.github.binaryfoo.gclog.Parser._
import org.joda.time.{DateTime, DateTimeZone}
import org.scalatest.{FlatSpec, Matchers}

class ParserTest extends FlatSpec with Matchers {

  "Full GC" should "be matched" in {
    val line = """2015-12-04T16:07:12.422+1100: 6994.482: [Full GC [PSYoungGen: 14194K->0K(1376448K)] [ParOldGen: 2788303K->1802287K(2796224K)] 2802498K->1802287K(4172672K) [PSPermGen: 66560K->66131K(132736K)], 3.8232380 secs] [Times: user=10.81 sys=0.06, real=3.83 secs]"""

    val Parsed.Success(value, _) = gcLine.parse(line)
    value.time shouldBe new DateTime(2015, 12, 4, 16, 7, 12, 422, DateTimeZone.forOffsetHours(11))
    value.gcType shouldBe "Full GC"
    value.heapDelta shouldBe SizeDelta("2802498K", "1802287K", "4172672K")
    value.pauseSeconds shouldBe 3.823238
  }

  "Timestamp" should "be parsed" in {
    val Parsed.Success(value, _) = Timestamp.parse("2015-12-04T16:07:12.422+1100")
    value shouldBe new DateTime(2015, 12, 4, 16, 7, 12, 422, DateTimeZone.forOffsetHours(11))
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
}
