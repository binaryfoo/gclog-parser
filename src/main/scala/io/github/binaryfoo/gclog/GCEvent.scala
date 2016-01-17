package io.github.binaryfoo.gclog

import org.joda.time.DateTime

trait GCEvent {
  def time: DateTime
  def jvmAgeMillis: Long
  def toSeq: Seq[(String, String)]
  def heap: Option[SizeDelta]
}
