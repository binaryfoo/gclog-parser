package io.github.binaryfoo.gclog

import fastparse.all._
import org.joda.time.format.DateTimeFormat

object Parser {

  val TimestampFormat = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ss.SSSZ").withOffsetParsed()

  val NotEndBracket = CharsWhile(_ != ']')

  val digit = P(CharIn('0' to '9'))
  val yyyyMMdd = digit.rep(4) ~ "-" ~ digit.rep(2) ~ "-" ~ digit.rep(2)
  val hhMMssSSS = digit.rep(2) ~ ":" ~ digit.rep(2) ~ ":" ~ digit.rep(2) ~ "." ~ digit.rep(3)
  val timezone = ("+" | "-") ~ digit.rep(4)
  val Timestamp = (yyyyMMdd ~ "T" ~ hhMMssSSS ~ timezone).!.map(TimestampFormat.parseDateTime)

  val Seconds = (digit.rep ~ "." ~ digit.rep).!.map(_.toDouble)

  val multiplier = CharIn(Seq('K', 'M'))
  val Size = digit.rep ~ multiplier
  val SizeStats = (Size.! ~ "->" ~ Size.! ~ "(" ~ Size.! ~ ")").map {
    case (start, end, capacity) => SizeDelta(start, end, capacity)
  }

  val IgnoredLine = CharsWhile(_ != '\n').? ~ "\n"
  val Ignored = "\nDesired" ~ IgnoredLine

  val GenerationName = CharIn('a' to 'z', 'A' to 'Z').rep
  val GenerationStats = ("[" ~ GenerationName.! ~ ": " ~ SizeStats ~ "]").map {
    case (name, delta) => GenerationDelta(name, delta)
  }
  val GcType = StringIn("Full GC", "GC--", "GC")
  val GcCause = " (" ~ CharIn('a' to 'z', 'A' to 'Z', ' ' to ' ').rep.! ~ ") "
  val collectionStats = "[" ~ GcType.! ~ GcCause.? ~ Ignored.? ~ " " ~ (GenerationStats | SizeStats).rep(sep = " ") ~ ", " ~ Seconds ~ " secs]"

  val gcLine = ((Timestamp ~ ": ").? ~ Seconds ~ ": " ~ collectionStats).map {
    case (timestamp, jvmAge, (gcType, gcCause, collections, pause)) =>
      val heapDelta = collections.collectFirst { case heap: SizeDelta => heap }.get
      val generationDeltas = collections.collect { case generation: GenerationDelta => generation }
      GCEvent(timestamp.orNull, jvmAge, gcType, gcCause.orNull, heapDelta, generationDeltas, pause)
  }

  val gcLog = (gcLine | IgnoredLine).rep

  def parseLog(log: String): Seq[GCEvent] = {
    val Parsed.Success(value, _) = gcLog.parse(log)
    value.collect { case e: GCEvent => e }
  }
}
