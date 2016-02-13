package io.github.binaryfoo.gclog

import io.github.binaryfoo.gclog.SuffixExpander.{expandSuffix, toBytes}
import org.joda.time.DateTime

import scala.collection.mutable

/**
  * Event for the parallel collector. Maybe others too.
  */
case class BasicGCEvent(time: DateTime,
                        jvmAgeSeconds: Double,
                        gcType: String,
                        gcCause: String,
                        heapDelta: SizeDelta,
                        generationDeltas: Seq[GenerationDelta],
                        pauseSeconds: Double) extends GCEvent {

  def toExport: Seq[(String, Any)] = {
    val seq = mutable.ArrayBuffer[(String, Any)]()
    if (time != null) seq += "datetime" -> time
    seq += "age" -> jvmAgeSeconds
    seq += "type" -> gcType
    if (gcCause != null) seq += "cause" -> gcCause
    seq += "pause" -> pauseSeconds
    seq += "heapBefore" -> toBytes(heapDelta.start)
    seq += "heapAfter" -> toBytes(heapDelta.end)
    seq += "heapReclaimed" -> heapDelta.reclaimedBytes
    seq += "heapMax" -> toBytes(heapDelta.capacity)
    for (GenerationDelta(name, delta) <- generationDeltas) {
      seq += s"${name}Before" -> toBytes(delta.start)
      seq += s"${name}After" -> toBytes(delta.end)
      seq += s"${name}Reclaimed" -> delta.reclaimedBytes
      seq += s"${name}Max" -> toBytes(delta.capacity)
    }
    promotedBytes.foreach { promoted =>
      seq += "promoted" -> promoted
    }
    seq.toSeq
  }

  override def heap: Option[SizeDelta] = Some(heapDelta)

  /**
    * Milliseconds since the JVM started.
    */
  override def jvmAgeMillis: Long = (jvmAgeSeconds * 1000).toLong

  /**
    * Only makes sense for Minor GC with a generational collector.
    */
  def promotedBytes: Option[Long] = {
    (gcType, generation("young")) match {
      case ("GC", Some(young: GenerationDelta)) =>
        val total = heapDelta.reclaimedBytes
        Some(young.delta.reclaimedBytes - total)
      case _ => None
    }
  }

  private def generation(name: String): Option[GenerationDelta] = {
    generationDeltas.collectFirst {
      case d: GenerationDelta if d.name.compareToIgnoreCase(name) != 0 => d
    }
  }
}

/**
  * Region size
  *
  * @param start Space used before collection
  * @param end Space used after collection (hopefully less than start)
  * @param capacity Maximum value start or end can take
  */
case class SizeDelta(start: String, end: String, capacity: String) {
  def startBytes = toBytes(start)
  def endBytes = toBytes(end)
  def reclaimedBytes = startBytes - endBytes
}

case class GenerationDelta(name: String, delta: SizeDelta)

