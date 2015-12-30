package io.github.binaryfoo.gclog

import fastparse.all._
import org.joda.time.format.DateTimeFormat

object Parser {

  private val TimestampFormat = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SSSZ").withOffsetParsed()
  private val Digit = CharIn('0' to '9')
  private val YyyyMMdd = Digit.rep(4) ~ "-" ~ Digit.rep(2) ~ "-" ~ Digit.rep(2)
  private val HhMMssSSS = Digit.rep(2) ~ ":" ~ Digit.rep(2) ~ ":" ~ Digit.rep(2) ~ "." ~ Digit.rep(3)
  private val Timezone = ("+" | "-") ~ Digit.rep(4)
  val Timestamp = (YyyyMMdd ~ "T" ~ HhMMssSSS ~ Timezone).!.map(TimestampFormat.parseDateTime)

  private val Number = Digit.rep ~ "." ~ Digit.rep
  private val Seconds = Number.!.map(_.toDouble)
  private val Multiplier = CharIn(Seq('K', 'M'))
  private val Size = Digit.rep ~ Multiplier
  val SizeStats = (Size.! ~ "->" ~ Size.! ~ "(" ~ Size.! ~ ")").map {
    case (start, end, capacity) => SizeDelta(start, end, capacity)
  }

  private val IgnoredLine = CharsWhile(_ != '\n').? ~ "\n"
  private val Ignored = ("\nDesired" ~ IgnoredLine) | ("- age" ~ IgnoredLine)

  private val GenerationName = CharIn('a' to 'z', 'A' to 'Z').rep
  val GenerationStats = ("[" ~ GenerationName.! ~ Ignored.rep.? ~ ": " ~ SizeStats ~ (", " ~ Number ~ " secs").? ~ "]").map {
    case (name, delta) => GenerationDelta(name, delta)
  }
  private val GcType = StringIn("Full GC", "GC--", "GC")
  private val GcCause = " (" ~ CharIn('a' to 'z', 'A' to 'Z', ' ' to ' ').rep.! ~ ") "
  private val CollectionStats = "[" ~ GcType.! ~ GcCause.? ~ (Number ~ ": ").? ~ Ignored.? ~ " ".? ~ (GenerationStats | SizeStats).rep(sep = StringIn(" ", ", ")) ~ ", " ~ Seconds ~ " secs]"

  val GcLine = ((Timestamp ~ ": ").? ~ Seconds ~ ": " ~ CollectionStats).map {
    case (timestamp, jvmAge, (gcType, gcCause, collections, pause)) =>
      val heapDelta = collections.collectFirst { case heap: SizeDelta => heap }.get
      val generationDeltas = collections.collect { case generation: GenerationDelta => generation }
      GCEvent(timestamp.orNull, jvmAge, gcType, gcCause.orNull, heapDelta, generationDeltas, pause)
  }

  private val GcLog = (GcLine | IgnoredLine).rep

  private val Space = " ".rep
  private val RegionName = (CharIn('a' to 'z', 'A' to 'Z', '-' to '-', ' ' to ' ') ~ !("total"|"used")).rep.!.map(_.trim)
  private val HeapRegionSubSpace = Space ~ GenerationName.! ~ Space ~ "space" ~ Space ~ Size.! ~ "," ~ Space ~ (Digit.rep ~ "%").! ~ " used" ~ IgnoredLine
  private val HeapRegionSubSpaces = HeapRegionSubSpace.map {
    case (name, capacity, used) => HeapRegion(name, capacity, used)
  }.rep
  val HeapStat = (Space ~ RegionName ~ Space ~ "total " ~ Size.! ~ ", used " ~ Size.! ~ IgnoredLine ~ HeapRegionSubSpaces).map {
    case (name, total, used, subSpaces) =>
      val interesting = subSpaces.collect {
        case r@HeapRegion(subspace, c, u, _) if subspace != "object" => r
      }
      HeapRegion(name, total, used, interesting)
  }
  private val MetaspaceSubSpace = (Space ~ RegionName ~ Space ~ "used " ~ Size.! ~ ", capacity " ~ Size.! ~ IgnoredLine).map {
    case (name, used, capacity) => HeapRegion(name, capacity, used)
  }
  val MetaspaceStat = MetaspaceSubSpace.rep.filter(_.nonEmpty).map {
    case spaces =>
      spaces.head.copy(subspaces = spaces.tail)
  }
  private def heapDetails(when: String) = "Heap " ~ when ~ IgnoredLine ~ (HeapStat | MetaspaceStat).rep
  private val DetailedEvent = "{" ~ heapDetails("before") ~ GcLine ~ IgnoredLine.? ~ heapDetails("after") ~ "}"

  def parseLog(log: String): Seq[GCEvent] = {
    val Parsed.Success(value, _) = GcLog.parse(log)
    value.collect { case e: GCEvent => e }
  }

  /**
    * Use to parse data written when -XX:+PrintHeapAtGC option is passed to the JVM.
    */
  def parseWithHeapStats(log: String): Seq[DetailedGCEvent] = {
    val Parsed.Success(value, _) = (DetailedEvent | IgnoredLine).rep.parse(log)
    value.collect {
      case (before: Seq[HeapRegion], e: GCEvent, after: Seq[HeapRegion]) =>
        val deltas = before.zip(after)
          .flatMap {
            case (b, a) =>
              Seq((b, a)) ++ b.subspaces.zip(a.subspaces)
          }
          .map {
          case (b, a) if b.name == a.name =>
            RegionDelta(b.name, b.used, a.used, b.capacity, a.capacity)
        }
        DetailedGCEvent(e, deltas)
    }
  }
}
