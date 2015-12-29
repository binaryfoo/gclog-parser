package io.github.binaryfoo.gclog

case class DetailedGCEvent(e: GCEvent, regions: Seq[RegionDelta])

case class HeapRegion(name: String, capacity: String, used: String, subspaces: Seq[HeapRegion] = Seq.empty)

case class RegionDelta(name: String, start: String, end: String, startCapacity: String, endCapacity: String)
