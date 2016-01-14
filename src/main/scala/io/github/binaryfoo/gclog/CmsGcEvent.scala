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
  override def toSeq: Seq[(String, String)] = {
    val seq = mutable.ArrayBuffer[(String, String)]()
    if (time != null) seq += "datetime" -> time.toString("yyyy-MM-dd HH:mm:ss.SSS")
    seq += "age" -> jvmAgeSeconds.toString
    seq += "type" -> gcType
    if (gcCause != null) seq += "cause" -> gcCause
    seq += "pause" -> pauseSeconds.toString
    seq.toSeq
  }

}
