package io.github.binaryfoo.gclog

import org.joda.time.DateTime

import scala.collection.mutable

/**
  * Some support for the concurrent mark and sweep collector logs.
  * java -XX:+UseConcMarkSweepGC
  */
case class CmsGcEvent(time: DateTime,
                      jvmAgeSeconds: Double,
                      gcType: String,
                      gcCause: String,
                      pauseSeconds: Double) extends GCEvent {

  override def toExport: Seq[(String, Any)] = {
    val seq = mutable.ArrayBuffer[(String, Any)]()
    if (time != null) seq += "datetime" -> time
    seq += "age" -> jvmAgeSeconds
    seq += "type" -> gcType
    if (gcCause != null) seq += "cause" -> gcCause
    seq += "pause" -> pauseSeconds
    seq.toSeq
  }

  override def heap: Option[SizeDelta] = None

  override def jvmAgeMillis: Long = (jvmAgeSeconds * 1000).toLong
}
