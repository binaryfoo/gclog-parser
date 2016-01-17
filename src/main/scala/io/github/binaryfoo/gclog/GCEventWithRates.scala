package io.github.binaryfoo.gclog

import org.joda.time.DateTime

/**
  * Adds bytes allocated since last event, allocation rate per millisecond
  */
case class GCEventWithRates(base: GCEvent,
                            bytesAllocatedSinceLastEvent: Long,
                            millisSinceLastEvent: Long) extends GCEvent {
  override def time: DateTime = base.time
  override def gcType: String = base.gcType
  override def toSeq: Seq[(String, String)] = {
    base.toSeq ++ Seq(
      "heapAllocated" -> bytesAllocatedSinceLastEvent.toString,
      "heapAllocationRate" -> heapAllocationRate.toString,
      "promotionRate" -> promotionRate.toString
    )
  }
  override def heap: Option[SizeDelta] = base.heap
  override def jvmAgeMillis: Long = base.jvmAgeMillis

  /**
    * Bytes allocated per millisecond over the period between the previous event and this one.
    */
  def heapAllocationRate: Long = {
    if (millisSinceLastEvent == 0)
      0
    else
      bytesAllocatedSinceLastEvent / millisSinceLastEvent
  }

  /**
    * Bytes promoted from young to old generation during this collection expressed as a rate (per millisecond)
    * over the period between the previous event and this one.
    */
  def promotionRate: Long = {
    val promoted = base match {
      case b: BasicGCEvent if millisSinceLastEvent != 0 =>
        b.promotedBytes
      case b: DetailedGCEvent if millisSinceLastEvent != 0 =>
        b.e.promotedBytes
      case _ => None
    }
    promoted.map(_ / millisSinceLastEvent).getOrElse(0L)
  }
}
