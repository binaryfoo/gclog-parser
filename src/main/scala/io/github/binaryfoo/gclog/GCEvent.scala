package io.github.binaryfoo.gclog

import org.joda.time.DateTime

/**
  * A point in time when the JVM did something to do with garbage collection.
  */
trait GCEvent {

  def time: DateTime

  def gcType: String

  def jvmAgeMillis: Long

  def toSeq: Seq[(String, String)] = {
    toExport.map {
      case pair@(_, value: DateTime) => pair.copy(_2 = value.toString("yyyy-MM-dd HH:mm:ss.SSS"))
      case pair@(_, value) => pair.copy(_2 = value.toString)
    }
  }

  /**
    * A little loose. Intended to support publishing results to elastic search
    * @return A sequence of (name,value) attributes from the event.
    *         Eg: time, pause milliseconds, sizes before and after.
    */
  def toExport: Seq[(String, Any)]

  def heap: Option[SizeDelta]
}
