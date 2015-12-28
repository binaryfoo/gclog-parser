package io.github.binaryfoo.gclog

import org.joda.time.DateTime

case class GCEvent(time: DateTime,
                   gcType: String,
                   heapDelta: SizeDelta,
                   generationDeltas: Seq[GenerationDelta],
                   pauseSeconds: Double)

case class SizeDelta(start: String, end: String, capacity: String)

case class GenerationDelta(name: String, delta: SizeDelta)
