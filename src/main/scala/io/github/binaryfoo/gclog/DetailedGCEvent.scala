package io.github.binaryfoo.gclog

import SuffixExpander.toBytes
import org.joda.time.DateTime

/**
  * Captures extra data data written when -XX:+PrintHeapAtGC option is passed to the JVM.
  */
case class DetailedGCEvent(e: BasicGCEvent, regions: Seq[RegionDelta]) extends GCEvent {

  override def time: DateTime = e.time
  override def gcType: String = e.gcType
  override def toExport: Seq[(String, Any)] = {
    (e.toExport ++ regions.flatMap { case RegionDelta(name, start, end, startCapacity, endCapacity) =>
      Seq(
        s"${name}Before" -> toBytes(start),
        s"${name}After" -> toBytes(end),
        s"${name}CapacityBefore" -> toBytes(startCapacity),
        s"${name}CapacityAfter" -> toBytes(endCapacity)
      )
    }).distinct
  }

  override def heap: Option[SizeDelta] = e.heap
  override def jvmAgeMillis: Long = e.jvmAgeMillis
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
