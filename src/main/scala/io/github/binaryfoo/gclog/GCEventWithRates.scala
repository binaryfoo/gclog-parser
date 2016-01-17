package io.github.binaryfoo.gclog

import org.joda.time.DateTime

/**
  * Adds bytes allocated since last event, allocation rate per millisecond
  */
case class GCEventWithRates(base: GCEvent,
                            bytesAllocatedSinceLastEvent: Long,
                            millisSinceLastEvent: Long) extends GCEvent {
  override def time: DateTime = base.time
  override def toSeq: Seq[(String, String)] = {
    base.toSeq ++ Seq(
      "heapAllocated" -> bytesAllocatedSinceLastEvent.toString,
      "heapAllocationRate" -> heapAllocationRate.toString
    )
  }
  override def heap: Option[SizeDelta] = base.heap
  override def jvmAgeMillis: Long = base.jvmAgeMillis

  /**
    * Bytes allocated per millisecond in the period between the previous event and this one.
    */
  def heapAllocationRate: Long = {
    if (millisSinceLastEvent == 0)
      0
    else
      bytesAllocatedSinceLastEvent / millisSinceLastEvent
  }
}
