package io.github.binaryfoo.gclog

import org.joda.time.DateTime

import scala.collection.mutable

object AppPausedEvent {
  val GcType = "AppStopped"
}

case class AppPausedEvent(time: DateTime, jvmAgeSeconds: Double, stoppedSeconds: Double) extends GCEvent {
  override def gcType: String = AppPausedEvent.GcType

  override def jvmAgeMillis: Long = (jvmAgeSeconds * 1000).toLong

  override def toExport: Seq[(String, Any)] = {
    val seq = mutable.ArrayBuffer[(String, Any)]()
    if (time != null) seq += "datetime" -> time
    seq += "age" -> jvmAgeSeconds
    seq += "type" -> gcType
    seq += "stoppedSeconds" -> stoppedSeconds
    seq
  }

  override def heap: Option[SizeDelta] = None
}
