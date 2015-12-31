package io.github.binaryfoo.gclog

import io.github.binaryfoo.gclog.SuffixExpander.expandSuffix
import org.joda.time.DateTime

import scala.collection.mutable

case class BasicGCEvent(time: DateTime,
                        jvmAgeSeconds: Double,
                        gcType: String,
                        gcCause: String,
                        heapDelta: SizeDelta,
                        generationDeltas: Seq[GenerationDelta],
                        pauseSeconds: Double) extends GCEvent {

  override def toSeq: Seq[(String, String)] = {
    val seq = mutable.ArrayBuffer[(String, String)]()
    if (time != null) seq += "time" -> time.toString("yyyy-MM-dd HH:mm:ss.SSS")
    seq += "age" -> jvmAgeSeconds.toString
    seq += "type" -> gcType
    if (gcCause != null) seq += "cause" -> gcCause
    seq += "pause" -> pauseSeconds.toString
    seq += "heapBefore" -> expandSuffix(heapDelta.start)
    seq += "heapAfter" -> expandSuffix(heapDelta.end)
    seq += "heapMax" -> expandSuffix(heapDelta.capacity)
    for (GenerationDelta(name, delta) <- generationDeltas) {
      seq += s"${name}Before" -> expandSuffix(delta.start)
      seq += s"${name}After" -> expandSuffix(delta.end)
      seq += s"${name}Max" -> expandSuffix(delta.capacity)
    }
    seq.toSeq
  }

}

case class SizeDelta(start: String, end: String, capacity: String)

case class GenerationDelta(name: String, delta: SizeDelta)

