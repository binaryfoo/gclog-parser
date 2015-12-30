package io.github.binaryfoo.gclog

import fastparse.all._
import org.joda.time.format.DateTimeFormat

object Parser {

  val TimestampFormat = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SSSZ").withOffsetParsed()

  val digit = P(CharIn('0' to '9'))
  val yyyyMMdd = digit.rep(4) ~ "-" ~ digit.rep(2) ~ "-" ~ digit.rep(2)
  val hhMMssSSS = digit.rep(2) ~ ":" ~ digit.rep(2) ~ ":" ~ digit.rep(2) ~ "." ~ digit.rep(3)
  val timezone = ("+" | "-") ~ digit.rep(4)
  val Timestamp = (yyyyMMdd ~ "T" ~ hhMMssSSS ~ timezone).!.map(TimestampFormat.parseDateTime)

  val Number = digit.rep ~ "." ~ digit.rep
  val Seconds = Number.!.map(_.toDouble)

  val multiplier = CharIn(Seq('K', 'M'))
  val Size = digit.rep ~ multiplier
  val SizeStats = (Size.! ~ "->" ~ Size.! ~ "(" ~ Size.! ~ ")").map {
    case (start, end, capacity) => SizeDelta(start, end, capacity)
  }

  val IgnoredLine = CharsWhile(_ != '\n').? ~ "\n"
  val Ignored = ("\nDesired" ~ IgnoredLine) | ("- age" ~ IgnoredLine)

  val GenerationName = CharIn('a' to 'z', 'A' to 'Z').rep
  val GenerationStats = ("[" ~ GenerationName.! ~ Ignored.rep.? ~ ": " ~ SizeStats ~ (", " ~ Number ~ " secs").? ~ "]").map {
    case (name, delta) => GenerationDelta(name, delta)
  }
  val GcType = StringIn("Full GC", "GC--", "GC")
  val GcCause = " (" ~ CharIn('a' to 'z', 'A' to 'Z', ' ' to ' ').rep.! ~ ") "
  val collectionStats = "[" ~ GcType.! ~ GcCause.? ~ (Number ~ ": ").? ~ Ignored.? ~ " ".? ~ (GenerationStats | SizeStats).rep(sep = StringIn(" ", ", ")) ~ ", " ~ Seconds ~ " secs]"

  val gcLine = ((Timestamp ~ ": ").? ~ Seconds ~ ": " ~ collectionStats).map {
    case (timestamp, jvmAge, (gcType, gcCause, collections, pause)) =>
      val heapDelta = collections.collectFirst { case heap: SizeDelta => heap }.get
      val generationDeltas = collections.collect { case generation: GenerationDelta => generation }
      GCEvent(timestamp.orNull, jvmAge, gcType, gcCause.orNull, heapDelta, generationDeltas, pause)
  }

  val gcLog = (gcLine | IgnoredLine).rep

  val Space = " ".rep
  val RegionName = (CharIn('a' to 'z', 'A' to 'Z', '-' to '-', ' ' to ' ') ~ !("total"|"used")).rep.!.map(_.trim)
  val HeapRegionSubSpace = Space ~ GenerationName.! ~ Space ~ "space" ~ Space ~ Size.! ~ "," ~ Space ~ (digit.rep ~ "%").! ~ " used" ~ IgnoredLine
  val HeapRegionSubSpaces = HeapRegionSubSpace.map {
    case (name, capacity, used) => HeapRegion(name, capacity, used)
  }.rep
  val HeapStat = (Space ~ RegionName ~ Space ~ "total " ~ Size.! ~ ", used " ~ Size.! ~ IgnoredLine ~ HeapRegionSubSpaces).map {
    case (name, total, used, subSpaces) =>
      val interesting = subSpaces.collect {
        case r@HeapRegion(subspace, c, u, _) if subspace != "object" => r
      }
      HeapRegion(name, total, used, interesting)
  }
  val MetaspaceSubSpace = (Space ~ RegionName ~ Space ~ "used " ~ Size.! ~ ", capacity " ~ Size.! ~ IgnoredLine).map {
    case (name, used, capacity) => HeapRegion(name, capacity, used)
  }
  val MetaspaceStat = MetaspaceSubSpace.rep.filter(_.nonEmpty).map {
    case spaces =>
      spaces.head.copy(subspaces = spaces.tail)
  }
  private def heapDetails(when: String) = "Heap " ~ when ~ IgnoredLine ~ (HeapStat | MetaspaceStat).rep
  val DetailedEvent = "{" ~ heapDetails("before") ~ gcLine ~ IgnoredLine.? ~ heapDetails("after") ~ "}"

  def parseLog(log: String): Seq[GCEvent] = {
    val Parsed.Success(value, _) = gcLog.parse(log)
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
