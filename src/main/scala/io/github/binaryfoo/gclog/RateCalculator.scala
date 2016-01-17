package io.github.binaryfoo.gclog

class RateCalculator {

  private var previous: GCEvent = null

  def apply(events: Seq[GCEvent]): Seq[GCEventWithRates] = {
    for (e <- events) yield apply(e)
  }

  def apply(e: GCEvent): GCEventWithRates = {
    val allocated = if (previous == null) {
      e.heap.map(d => SuffixExpander.toBytes(d.start)).getOrElse(0L)
    } else {
      bytesAllocatedSince(e, previous)
    }
    val elapsedMillis = if (previous == null) {
      e.jvmAgeMillis
    } else {
      e.jvmAgeMillis - previous.jvmAgeMillis
    }
    previous = e
    new GCEventWithRates(e, allocated, elapsedMillis)
  }

  def addRates(e: GCEvent): GCEventWithRates = apply(e)

  private def bytesAllocatedSince(current: GCEvent, previous: GCEvent): Long = {
    (current.heap, previous.heap) match {
      case (Some(SizeDelta(currentHeap, _, _)),Some(SizeDelta(_, previousHeap, _))) =>
        SuffixExpander.toBytes(currentHeap) - SuffixExpander.toBytes(previousHeap)
      case _ =>
        0
    }
  }

}
