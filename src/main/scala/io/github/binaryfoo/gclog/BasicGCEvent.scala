package io.github.binaryfoo.gclog

import io.github.binaryfoo.gclog.SuffixExpander.{expandSuffix, toBytes}
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
    if (time != null) seq += "datetime" -> time.toString("yyyy-MM-dd HH:mm:ss.SSS")
    seq += "age" -> jvmAgeSeconds.toString
    seq += "type" -> gcType
    if (gcCause != null) seq += "cause" -> gcCause
    seq += "pause" -> pauseSeconds.toString
    seq += "heapBefore" -> expandSuffix(heapDelta.start)
    seq += "heapAfter" -> expandSuffix(heapDelta.end)
    seq += "heapReclaimed" -> heapDelta.reclaimedBytes.toString
    seq += "heapMax" -> expandSuffix(heapDelta.capacity)
    for (GenerationDelta(name, delta) <- generationDeltas) {
      seq += s"${name}Before" -> expandSuffix(delta.start)
      seq += s"${name}After" -> expandSuffix(delta.end)
      seq += s"${name}Reclaimed" -> delta.reclaimedBytes.toString
      seq += s"${name}Max" -> expandSuffix(delta.capacity)
    }
    seq.toSeq
  }

  override def heap: Option[SizeDelta] = Some(heapDelta)

  override def jvmAgeMillis: Long = (jvmAgeSeconds * 1000).toLong
}

case class SizeDelta(start: String, end: String, capacity: String) {
  def startBytes = toBytes(start)
  def endBytes = toBytes(end)
  def reclaimedBytes = startBytes - endBytes
}

case class GenerationDelta(name: String, delta: SizeDelta)

