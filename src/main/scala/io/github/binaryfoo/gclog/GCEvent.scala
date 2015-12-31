package io.github.binaryfoo.gclog

import org.joda.time.DateTime

trait GCEvent {
  def time: DateTime
  def toSeq: Seq[(String, String)]
}
