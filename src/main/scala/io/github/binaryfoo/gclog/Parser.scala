package io.github.binaryfoo.gclog

import fastparse.all._
import org.joda.time.format.DateTimeFormat

object Parser {

  private val TimestampFormat = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SSSZ").withOffsetParsed()
  private val YyyyMMdd = Digits(4) ~ "-" ~ Digits(2) ~ "-" ~ Digits(2)
  private val HhMMssSSS = Digits(2) ~ ":" ~ Digits(2) ~ ":" ~ Digits(2) ~ "." ~ Digits(3)
  private val Timezone = ("+" | "-") ~ Digits(4)
  val Timestamp = (YyyyMMdd ~ "T" ~ HhMMssSSS ~ Timezone).!.map(TimestampFormat.parseDateTime)

  private val Number = AtLeastDigits(1) ~ "." ~ AtLeastDigits(1)
  private val Seconds = Number.!.map(_.toDouble)
  private val Multiplier = CharIn(Seq('K', 'M'))
  private val Size = AtLeastDigits(1) ~ Multiplier
  val SizeStats = (Size.! ~ "->" ~ Size.! ~ "(" ~ Size.! ~ ")").map {
    case (start, end, capacity) => SizeDelta(start, end, capacity)
  }

  private val IgnoredLine = CharsWhile(_ != '\n').? ~ "\n"
  private val IgnoredTenuringTable = "- age" ~ IgnoredLine
  private val DesiredSurvivorSize = ("\nDesired survivor size " ~ AtLeastDigits(1).! ~ " bytes, new threshold " ~ AtLeastDigits(1).! ~ " (" ~ IgnoredLine ~ IgnoredTenuringTable.rep).map {
    case (desiredSize, newThreshold) => TenuringDistribution(desiredSize, newThreshold)
  }

  private val GenerationName = CharIn('a' to 'z', 'A' to 'Z', '0' to '9', Seq(' '), Seq('-')).rep
  val GenerationStats = ((Number ~ ": ").? ~ "[" ~ GenerationName.! ~ DesiredSurvivorSize.? ~ ": " ~ SizeStats ~ (", " ~ Number ~ " secs").? ~ "]").map {
    case (name, strayTenuringDistribution, delta) => (GenerationDelta(name, delta), strayTenuringDistribution)
  }
  private val GcType = CharIn('a' to 'z', 'A' to 'Z', Seq('-'), Seq(' ')).rep.!.map(_.trim)
  private val GcCause = "(" ~ CharIn('a' to 'z', 'A' to 'Z', ' ' to ' ').rep.! ~ ")" ~ " ".rep
  private val Java8PromotionFailureFlag = "--".!
  private val BasicEvent = ((Number ~ ": ").? ~ " ".? ~ (GenerationStats | SizeStats).rep(sep = StringIn(" ", ", ") | Pass) ~ ", " ~ Seconds ~ " secs]").map {
    case (collections, pause) =>
      val heapDelta = collections.collectFirst { case heap: SizeDelta => heap }.get
      val generationDeltas = collections.collect { case (generation: GenerationDelta, _) => generation }
      val strayTenuringDistribution = collections.collectFirst { case (_, Some(d: TenuringDistribution)) => d }
      (heapDelta, generationDeltas, pause, strayTenuringDistribution)
  }
  private def basicEvent(gcType: String, gcCause: Option[String], tenuringDistribution: Option[TenuringDistribution]): P[BasicGCEvent] = {
    BasicEvent.map {
      case (heapDelta, generationDeltas, pause, strayTenuringDistribution) =>
        BasicGCEvent(null, 0, gcType, gcCause.orNull, heapDelta, generationDeltas, pause, tenuringDistribution.orElse(strayTenuringDistribution))
    }
  }
  private val CmsEvent = "]" | ((AnyChar ~ !"real=").rep ~ " real=" ~ Seconds ~ " secs]")
  private def cmsEvent(gcType: String, gcCause: Option[String]) = {
    CmsEvent.map {
      case (pause: Double) => CmsGcEvent(null, 0, gcType, gcCause.orNull, pause)
      case (_) => CmsGcEvent(null, 0, gcType, gcCause.orNull, 0)
    }
  }
  private val CollectionStats = "[" ~/ (GcType ~ GcCause.? ~ Java8PromotionFailureFlag.? ~ DesiredSurvivorSize.?).flatMap {
    case (gcType, None, _, _) if gcType.startsWith("CMS") => cmsEvent(gcType, None)
    case (gcType, c@Some(gcCause), _, _) if gcCause.startsWith("CMS") => cmsEvent(gcType, c)
    case (gcType, gcCause, typePart2, tenuringDistribution) => basicEvent(gcType + typePart2.getOrElse(""), gcCause, tenuringDistribution)
  }
  private val TotalAppStoppedTime = "Total time for which application threads were stopped: " ~ Seconds ~ " seconds, Stopping threads took: " ~ Seconds ~ " seconds" ~ "\n".?
  private val AppStoppedEvent = ((Timestamp ~ ": ").? ~ Seconds ~ ": " ~ TotalAppStoppedTime).map {
    case (timestamp, jvmAge, (stoppedTime: Double, secondsToStop: Double)) =>
      AppPausedEvent(timestamp.orNull, jvmAge, stoppedTime, secondsToStop)
  }

  val GcLine = ((Timestamp ~ ": ").? ~ Seconds ~ ": " ~/ (CollectionStats | TotalAppStoppedTime)).map {
    case (timestamp, jvmAge, b: BasicGCEvent) =>
      b.copy(time = timestamp.orNull, jvmAgeSeconds = jvmAge)
    case (timestamp, jvmAge, c: CmsGcEvent) =>
      c.copy(time = timestamp.orNull, jvmAgeSeconds = jvmAge)
    case (timestamp, jvmAge, (stoppedTime: Double, secondsToStop: Double)) =>
      AppPausedEvent(timestamp.orNull, jvmAge, stoppedTime, secondsToStop)
  }

  private val GcLog = (GcLine | IgnoredLine).rep

  private val Space = " ".rep
  private val RegionName = (CharIn('a' to 'z', 'A' to 'Z', '-' to '-', ' ' to ' ') ~ !("total"|"used")).rep.!.map(_.trim)
  private val SubSpaceName = CharIn('a' to 'z', 'A' to 'Z').rep
  private val HeapRegionSubSpace = Space ~ SubSpaceName.! ~ Space ~ "space" ~ Space ~ Size.! ~ "," ~ Space ~ (AtLeastDigits(1) ~ "%").! ~ " used" ~ IgnoredLine
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
  private val DetailedEvent = ("{" ~/ heapDetails("before") ~ GcLine ~ IgnoredLine.? ~ heapDetails("after") ~ "}").map {
    case (before: Seq[HeapRegion], e: BasicGCEvent, after: Seq[HeapRegion]) =>
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
  private val QuickDetailedEvent = ("{" ~/ EndsWithCurlyBracket).!.map { s =>
    val Parsed.Success(value, _) = DetailedEvent.parse(s)
    value
  }

  private val IncrementalParser = GcLine | QuickDetailedEvent

  def parseLog[T <: GCEvent](log: String): Seq[T] = {
    val Parsed.Success(value, _) = GcLog.parse(log)
    value.collect { case e: T => e }
  }

  /**
    * Use to parse data written when -XX:+PrintHeapAtGC option is passed to the JVM.
    */
  def parseWithHeapStats(log: String): Seq[GCEvent] = {
    val Parsed.Success(value, _) = (DetailedEvent | AppStoppedEvent | IgnoredLine).rep.parse(log)
    value.collect {
      case e: GCEvent => e
    }
  }

  def incrementalParse(lines: String): IncrementalResult = {
    IncrementalParser.parse(lines) match {
      case Parsed.Success(value: GCEvent, _) => GcEventParsed(value)
      case f@Parsed.Failure(lastParser, index, _) =>
        // heuristic: if we've matched at least half the first line assume we're gonna match
        val halfwayMark = (lines.indexOf('\n') match {
          case -1 => lines.length
          case x => x
        }) / 2
        if (index <= halfwayMark ) SkipLine else NeedAnotherLine
    }
  }
}

sealed trait IncrementalResult
object SkipLine extends IncrementalResult
object NeedAnotherLine extends IncrementalResult
case class GcEventParsed(event: GCEvent) extends IncrementalResult


