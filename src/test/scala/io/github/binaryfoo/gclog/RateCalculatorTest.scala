package io.github.binaryfoo.gclog

class RateCalculatorTest extends GcLogTest {

  "Heap allocated since last event" should "be calculated" in {
    val events = Parser.parseLog(testInput("basic-java7-gc.log"))
    val calculated = new RateCalculator().apply(events)

    calculated(0).bytesAllocatedSinceLastEvent shouldBe 66048 * 1024
    calculated(0).millisSinceLastEvent shouldBe 263
    calculated(0).heapAllocationRate shouldBe (66048 * 1024) / 263
    calculated(0).heap.get.reclaimedBytes shouldBe (66048 - 59108) * 1024

    calculated(1).bytesAllocatedSinceLastEvent shouldBe (125156 * 1024 - 59108 * 1024)
    calculated(1).millisSinceLastEvent shouldBe 341 - 263
    calculated(1).heapAllocationRate shouldBe (125156 * 1024 - 59108 * 1024) / (341 - 263)
    calculated(1).heap.get.reclaimedBytes shouldBe (125156 - 122164) * 1024

    calculated(2).bytesAllocatedSinceLastEvent shouldBe 0 // minor collection freed nothing
    calculated(2).millisSinceLastEvent shouldBe 371 - 341

    calculated(3).bytesAllocatedSinceLastEvent shouldBe (251862 * 1024 - 119766 * 1024)
    calculated(3).millisSinceLastEvent shouldBe 727 - 371
  }

  it should "export calculated fields" in {
    val events = Parser.parseLog(testInput("basic-java8-gc.log"))
    val calculated = new RateCalculator().apply(events)

    calculated(1).toSeq.mkString("\n") shouldBe """(age,0.299)
                                                  |(type,GC)
                                                  |(cause,Allocation Failure)
                                                  |(pause,0.0292072)
                                                  |(heapBefore,125737984)
                                                  |(heapAfter,122723328)
                                                  |(heapReclaimed,3014656)
                                                  |(heapMax,324534272)
                                                  |(heapAllocated,67108864)
                                                  |(heapAllocationRate,1048576)""".stripMargin
  }

  it should "be calculated with heap stats" in {
    val events = Parser.parseWithHeapStats(testInput("heap-java7-gc.log"))
    val calculator = new RateCalculator
    calculator.addRates(events(0)).bytesAllocatedSinceLastEvent shouldBe 66048 * 1024
    calculator.addRates(events(1)).bytesAllocatedSinceLastEvent shouldBe (125149 * 1024 - 59101 * 1024)
  }
}
