package io.github.binaryfoo.gclog

import org.joda.time.DateTime

case class GCEvent(time: DateTime,
                   jvmAgeSeconds: Double,
                   gcType: String,
                   heapDelta: SizeDelta,
                   generationDeltas: Seq[GenerationDelta],
                   pauseSeconds: Double)

case class SizeDelta(start: String, end: String, capacity: String)

case class GenerationDelta(name: String, delta: SizeDelta)

object SuffixExpander {
  def toBytes(v: String): Long = {
    val multiplier = v.charAt(v.length - 1) match {
      case 'K' => 1024
    }
    v.substring(0, v.length - 1).toLong * multiplier
  }
}
