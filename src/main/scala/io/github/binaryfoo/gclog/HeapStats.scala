package io.github.binaryfoo.gclog

import SuffixExpander.expandSuffix

/**
  * Captures extra data data written when -XX:+PrintHeapAtGC option is passed to the JVM.
  */
case class DetailedGCEvent(e: GCEvent, regions: Seq[RegionDelta]) {

  def toSeq: Seq[(String, String)] = {
    e.toSeq ++ regions.flatMap { case RegionDelta(name, start, end, startCapacity, endCapacity) =>
      Seq(
        s"${name}Before" -> expandSuffix(start),
        s"${name}After" -> expandSuffix(end),
        s"${name}CapacityBefore" -> expandSuffix(startCapacity),
        s"${name}CapacityAfter" -> expandSuffix(endCapacity)
      )
    }
  }
}

/**
  * For a given GC event there will be a block similar to the following for the before and after collection
  * cases. Each line in the following example will become an instance of RegionDelta.
  * <pre>
  *  PSYoungGen      total 76800K, used 66048K [0x00000007aaa80000, 0x00000007b0000000, 0x0000000800000000)
  *   eden space 66048K, 100% used [0x00000007aaa80000,0x00000007aeb00000,0x00000007aeb00000)
  *   from space 10752K, 0% used [0x00000007af580000,0x00000007af580000,0x00000007b0000000)
  *   to   space 10752K, 0% used [0x00000007aeb00000,0x00000007aeb00000,0x00000007af580000)
  * </pre>
  *
  * Capacity can be adjusted during a GC event.
  */
case class RegionDelta(name: String, start: String, end: String, startCapacity: String, endCapacity: String)

/**
  * Intermediate result on the way to producing a RegionDelta.
  *
  * In the following example PSYoungGen would be the HeapRegion's name, total == capacity, used == used.
  * Each of eden, from and to are subspaces.
  * <pre>
  *  PSYoungGen      total 76800K, used 66048K [0x00000007aaa80000, 0x00000007b0000000, 0x0000000800000000)
  *   eden space 66048K, 100% used [0x00000007aaa80000,0x00000007aeb00000,0x00000007aeb00000)
  *   from space 10752K, 0% used [0x00000007af580000,0x00000007af580000,0x00000007b0000000)
  *   to   space 10752K, 0% used [0x00000007aeb00000,0x00000007aeb00000,0x00000007af580000)
  * </pre>
  */
case class HeapRegion(name: String, capacity: String, used: String, subspaces: Seq[HeapRegion] = Seq.empty)
