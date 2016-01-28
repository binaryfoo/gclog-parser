package io.github.binaryfoo.gclog

import fastparse.all._
import io.github.binaryfoo.gclog.Parser._
import org.joda.time.{DateTime, DateTimeZone}

class ParserTest extends GcLogTest {

  private val Plus11 = DateTimeZone.forOffsetHours(11)

  "Full GC" should "be parsed" in {
    val line = """2015-12-04T16:07:12.422+1100: 6994.482: [Full GC [PSYoungGen: 14194K->0K(1376448K)] [ParOldGen: 2788303K->1802287K(2796224K)] 2802498K->1802287K(4172672K) [PSPermGen: 66560K->66131K(132736K)], 3.8232380 secs] [Times: user=10.81 sys=0.06, real=3.83 secs]"""

    val Parsed.Success(value: BasicGCEvent, _) = GcLine.parse(line)
    value.time shouldBe new DateTime(2015, 12, 4, 16, 7, 12, 422, Plus11)
    value.jvmAgeSeconds shouldBe 6994.482
    value.gcType shouldBe "Full GC"
    value.heapDelta shouldBe SizeDelta("2802498K", "1802287K", "4172672K")
    value.pauseSeconds shouldBe 3.823238
  }

  "GCEvent" should "be convertible into a Map" in {
    val line = """2015-12-04T16:07:12.422+1100: 6994.482: [Full GC [PSYoungGen: 14194K->0K(1376448K)] [ParOldGen: 2788303K->1802287K(2796224K)] 2802498K->1802287K(4172672K) [PSPermGen: 66560K->66131K(132736K)], 3.8232380 secs] [Times: user=10.81 sys=0.06, real=3.83 secs]"""

    val Parsed.Success(value, _) = GcLine.parse(line)
    value.toSeq.mkString("\n") shouldBe """(datetime,2015-12-04 16:07:12.422)
                                          |(age,6994.482)
                                          |(type,Full GC)
                                          |(pause,3.823238)
                                          |(heapBefore,2869757952)
                                          |(heapAfter,1845541888)
                                          |(heapReclaimed,1024216064)
                                          |(heapMax,4272816128)
                                          |(PSYoungGenBefore,14534656)
                                          |(PSYoungGenAfter,0)
                                          |(PSYoungGenReclaimed,14534656)
                                          |(PSYoungGenMax,1409482752)
                                          |(ParOldGenBefore,2855222272)
                                          |(ParOldGenAfter,1845541888)
                                          |(ParOldGenReclaimed,1009680384)
                                          |(ParOldGenMax,2863333376)
                                          |(PSPermGenBefore,68157440)
                                          |(PSPermGenAfter,67718144)
                                          |(PSPermGenReclaimed,439296)
                                          |(PSPermGenMax,135921664)""".stripMargin
  }

  "Minor collection" should "be convertible to a Map" in {
    val line = """2015-12-28T13:50:37.116-1000: 0.251: [GC (Allocation Failure) [PSYoungGen: 65536K->10736K(76288K)] 65536K->57253K(251392K), 0.0217970 secs] [Times: user=0.09 sys=0.06, real=0.02 secs]"""

    val Parsed.Success(value, _) = GcLine.parse(line)
    value.toSeq.mkString("\n") should include("""(promoted,47633408)""")
  }

  "CmsGcEvent" should "be convertible into a Map" in {
    val line = """2015-05-26T16:23:08.447-0200: 65.550: [GC (CMS Final Remark) [YG occupancy: 387920 K (613440 K)]65.550: [Rescan (parallel) , 0.0085125 secs]65.559: [weak refs processing, 0.0000243 secs]65.559: [class unloading, 0.0013120 secs]65.560: [scrub symbol table, 0.0008345 secs]65.561: [scrub string table, 0.0001759 secs][1 CMS-remark: 10812086K(11901376K)] 11200006K(12514816K), 0.0110730 secs] [Times: user=0.06 sys=0.00, real=0.01 secs]"""

    val Parsed.Success(value, _) = GcLine.parse(line)
    value.toSeq.mkString("\n") shouldBe """(datetime,2015-05-26 16:23:08.447)
                                          |(age,65.55)
                                          |(type,GC)
                                          |(cause,CMS Final Remark)
                                          |(pause,0.01)""".stripMargin
  }

  "Timestamp" should "be parsed" in {
    val Parsed.Success(value, _) = Timestamp.parse("2015-12-04T16:07:12.422+1100")

    value shouldBe new DateTime(2015, 12, 4, 16, 7, 12, 422, Plus11)
  }

  "Size stats" should "be parsed" in {
    val Parsed.Success(value, _) = SizeStats.parse("14194K->0K(1376448K)")
    value shouldBe SizeDelta("14194K", "0K", "1376448K")
  }

  "Generation stats" should "be parsed" in {
    val Parsed.Success(value, _) = GenerationStats.parse("[PSYoungGen: 14194K->0K(1376448K)]")
    value.name shouldBe "PSYoungGen"
    value.delta shouldBe SizeDelta("14194K", "0K", "1376448K")
  }

  "Promotion failure" should "be parsed" in {
    val line = """2015-12-10T15:42:08.076+1100: 523890.136: [GC-- [PSYoungGen: 1275256K->1275256K(1275264K)] 4007798K->4071477K(4071488K), 0.3913740 secs] [Times: user=0.54 sys=0.00, real=0.39 secs]"""

    val Parsed.Success(value: BasicGCEvent, _) = GcLine.parse(line)
    value.gcType shouldBe "GC--"
    value.pauseSeconds shouldBe 0.391374
  }

  "Multi-line young generation collection" should "be parsed" in {
    val lines = """2015-12-10T15:43:18.274+1100: 523960.334: [GC
      |Desired survivor size 129368064 bytes, new threshold 3 (max 15)
      | [PSYoungGen: 1220800K->88639K(1260480K)] 3440993K->2372792K(4056704K), 0.1104060 secs] [Times: user=0.24 sys=0.01, real=0.12 secs]
      |""".stripMargin

    val Parsed.Success(value: BasicGCEvent, _) = GcLine.parse(lines)
    value.time shouldBe new DateTime(2015, 12, 10, 15, 43, 18, 274, Plus11)
    value.heapDelta shouldBe SizeDelta("3440993K", "2372792K", "4056704K")
    value.generationDeltas shouldBe Seq(GenerationDelta("PSYoungGen", SizeDelta("1220800K", "88639K", "1260480K")))
    value.pauseSeconds shouldBe 0.110406
  }

  "Multiple events" should "be parsed" in {
    val events = Parser.parseLog[BasicGCEvent](testInput("fragment.txt"))
    events(0).time shouldBe new DateTime(2015, 12, 10, 15, 46, 54, 299, Plus11)
    events(0).gcType shouldBe "GC"

    events(1).time shouldBe new DateTime(2015, 12, 10, 15, 46, 54, 493, Plus11)
    events(1).gcType shouldBe "Full GC"
  }

  "GC cause" should "be parsed" in {
    val Parsed.Success(value: BasicGCEvent, _) = GcLine.parse("0.235: [GC (Allocation Failure)  65536K->57255K(251392K), 0.0222615 secs]")
    value.gcCause shouldBe "Allocation Failure"
  }

  "GC cause with details" should "be parsed" in {
    val Parsed.Success(value: BasicGCEvent, _) = GcLine.parse("2015-12-28T13:50:37.116-1000: 0.251: [GC (Allocation Failure) [PSYoungGen: 65536K->10736K(76288K)] 65536K->57253K(251392K), 0.0217970 secs] [Times: user=0.09 sys=0.06, real=0.02 secs]")
    value.gcCause shouldBe "Allocation Failure"
  }

  "Metaspace sizing" should "be parsed" in {
    val Parsed.Success(value: BasicGCEvent, _) = GcLine.parse("2015-12-28T13:50:37.214-1000: 0.349: [Full GC (Ergonomics) [PSYoungGen: 10720K->0K(141824K)] [ParOldGen: 109101K->117471K(290816K)] 119821K->117471K(432640K), [Metaspace: 4082K->4082K(1056768K)], 0.2284721 secs] [Times: user=1.42 sys=0.02, real=0.23 secs]")
    value.generationDeltas.collectFirst { case d: GenerationDelta if d.name == "Metaspace" => d }.get shouldBe GenerationDelta("Metaspace", SizeDelta("4082K", "4082K", "1056768K"))
  }

  "Basic jdk7 log" should "be parsed" in {
    val events = Parser.parseLog[BasicGCEvent](testInput("basic-java7-gc.log"))
    events.size shouldBe 7

    events(0).jvmAgeSeconds shouldBe 0.263
    events(0).pauseSeconds shouldBe 0.022292
    events(6).jvmAgeSeconds shouldBe 2.832
    events(6).pauseSeconds shouldBe 0.022377
  }

  "Detailed jdk7 log" should "be parsed" in {
    val events = Parser.parseLog(testInput("details-java7-gc.log"))
    events.size shouldBe 7
  }

  "Jdk7 log with heap stats" should "be parsed" in {
    val events = Parser.parseLog[BasicGCEvent](testInput("heap-java7-gc.log"))
    events.size shouldBe 7
    events.last.jvmAgeSeconds shouldBe 2.86
  }

  "Basic jdk8 log" should "be parsed" in {
    val events = Parser.parseLog[BasicGCEvent](testInput("basic-java8-gc.log"))
    events.size shouldBe 7

    events(0).jvmAgeSeconds shouldBe 0.235
    events(0).pauseSeconds shouldBe 0.0222615
    events(6).jvmAgeSeconds shouldBe 2.232
    events(6).pauseSeconds shouldBe 0.0209706
  }

  "Detailed jdk8 log" should "be parsed" in {
    val events = Parser.parseLog(testInput("details-java8-gc.log"))
    events.size shouldBe 7
  }

  "Jdk8 log with heap stats" should "be parsed" in {
    val events = Parser.parseLog[BasicGCEvent](testInput("heap-java8-gc.log"))
    events.size shouldBe 7
    events.last.jvmAgeSeconds shouldBe 2.198
  }

  "ParNew with -XX:+PrintTenuringDistribution" should "be parsed" in {
    val input = """7.524: [GC (Allocation Failure) 7.524: [ParNew
      |Desired survivor size 53673984 bytes, new threshold 6 (max 6)
      |- age   1:    5714984 bytes,    5714984 total
      |: 838848K->5616K(943680K), 0.0118666 secs] 838848K->5616K(943744K), 0.0119520 secs] [Times: user=0.01 sys=0.01, real=0.01 secs]
      |""".stripMargin

    val events = Parser.parseLog[BasicGCEvent](input)
    events.head.jvmAgeSeconds shouldBe 7.524
    events.head.pauseSeconds shouldBe 0.011952
  }

  "CMS Full GC" should "be parsed" in {
    val input = """29517.100: [Full GC (Allocation Failure) 29517.100: [CMS: 819199K->819199K(819200K), 3.2809595 secs] 1762879K->1762879K(1762880K), [Metaspace: 21995K->21995K(1069056K)], 3.2810538 secs] [Times: user=3.28 sys=0.00, real=3.28 secs]"""

    val events = Parser.parseLog[BasicGCEvent](input)
    events.head.jvmAgeSeconds shouldBe 29517.1
    events.head.pauseSeconds shouldBe 3.2810538
    events.head.generationDeltas.head shouldBe GenerationDelta("CMS", SizeDelta("819199K", "819199K", "819200K"))
  }

  "CMS region" should "be parsed" in {
    val input = "concurrent mark-sweep generation total 64K, used 0K [0x000000078e000000, 0x000000078e010000, 0x00000007c0000000)\n"

    val Parsed.Success(value, _) = HeapStat.parse(input)
    value.name shouldBe "concurrent mark-sweep generation"
    value.capacity shouldBe "64K"
    value.used shouldBe "0K"
    value.subspaces shouldBe empty
  }

  "Metaspace region" should "be parsed" in {
    val input = """ Metaspace       used 12441K, capacity 12616K, committed 12928K, reserved 1060864K
                  |  class space    used 1488K, capacity 1562K, committed 1664K, reserved 1048576K
                  |""".stripMargin

    val Parsed.Success(value, _) = MetaspaceStat.parse(input)
    value.name shouldBe "Metaspace"
    value.capacity shouldBe "12616K"
    value.used shouldBe "12441K"
    value.subspaces shouldBe Seq(HeapRegion("class space", "1562K", "1488K"))
  }

  "par new region" should "be parsed" in {
    val input = """ par new generation   total 943680K, used 5616K [0x000000072a000000, 0x0000000769ff0000, 0x000000078e000000)
                  |  eden space 838848K,   0% used [0x000000072a000000, 0x000000072a000000, 0x000000075d330000)
                  |  from space 104832K,   5% used [0x0000000763990000, 0x0000000763f0c180, 0x0000000769ff0000)
                  |  to   space 104832K,   0% used [0x000000075d330000, 0x000000075d330000, 0x0000000763990000)
                  |""".stripMargin

    val Parsed.Success(value, _) = HeapStat.parse(input)
    value.name shouldBe "par new generation"
    value.capacity shouldBe "943680K"
    value.used shouldBe "5616K"
    value.subspaces.size shouldBe 3
  }

  "ParOldGen heap region" should "be parsed" in {
    val input = """ ParOldGen       total 2796224K, used 2590524K [0x0000000700000000, 0x00000007aaab0000, 0x00000007aaab0000)
      |  object space 2796224K, 92% used [0x0000000700000000,0x000000079e1cf2a8,0x00000007aaab0000)
      |""".stripMargin

    val Parsed.Success(value, _) = HeapStat.parse(input)
    value.name shouldBe "ParOldGen"
    value.capacity shouldBe "2796224K"
    value.used shouldBe "2590524K"
    value.subspaces shouldBe empty
  }

  "PSYoungGen heap region" should "be parsed" in {
    val input = """ PSYoungGen      total 1070400K, used 1070376K [0x00000007aaab0000, 0x0000000800000000, 0x0000000800000000)
                  |  eden space 910272K, 100% used [0x00000007aaab0000,0x00000007e23a0000,0x00000007e23a0000)
                  |  from space 160128K, 99% used [0x00000007e23a0000,0x00000007ebffa2f8,0x00000007ec000000)
                  |  to   space 254848K, 0% used [0x00000007f0720000,0x00000007f0720000,0x0000000800000000)
                  |""".stripMargin

    val Parsed.Success(value, _) = HeapStat.parse(input)
    value.name shouldBe "PSYoungGen"
    value.capacity shouldBe "1070400K"
    value.used shouldBe "1070376K"
    value.subspaces(0).name shouldBe "eden"
    value.subspaces(0).capacity shouldBe "910272K"
    value.subspaces(0).used shouldBe "100%"
    value.subspaces(1).name shouldBe "from"
    value.subspaces(1).capacity shouldBe "160128K"
    value.subspaces(1).used shouldBe "99%"
    value.subspaces(2).name shouldBe "to"
    value.subspaces(2).capacity shouldBe "254848K"
    value.subspaces(2).used shouldBe "0%"
  }

  "Heap statistics" should "be parseable" in {
    val events = Parser.parseWithHeapStats(testInput("fragment.txt"))
    events.size shouldBe 2
    events(0).regions.mkString("\n") shouldBe """RegionDelta(PSYoungGen,1070376K,76319K,1070400K,1155840K)
                                                |RegionDelta(eden,100%,0%,910272K,900992K)
                                                |RegionDelta(from,99%,29%,160128K,254848K)
                                                |RegionDelta(to,0%,0%,254848K,242240K)
                                                |RegionDelta(ParOldGen,2590524K,2731841K,2796224K,2796224K)
                                                |RegionDelta(PSPermGen,67601K,67601K,67648K,67648K)""".stripMargin

    events(1).toSeq.mkString("\n") shouldBe """(datetime,2015-12-10 15:46:54.493)
                                              |(age,524176.553)
                                              |(type,Full GC)
                                              |(pause,2.324499)
                                              |(heapBefore,2875555840)
                                              |(heapAfter,2122715136)
                                              |(heapReclaimed,752840704)
                                              |(heapMax,4046913536)
                                              |(PSYoungGenBefore,78150656)
                                              |(PSYoungGenAfter,0)
                                              |(PSYoungGenReclaimed,78150656)
                                              |(PSYoungGenMax,1183580160)
                                              |(ParOldGenBefore,2797405184)
                                              |(ParOldGenAfter,2122715136)
                                              |(ParOldGenReclaimed,674690048)
                                              |(ParOldGenMax,2863333376)
                                              |(PSPermGenBefore,69223424)
                                              |(PSPermGenAfter,69223424)
                                              |(PSPermGenReclaimed,0)
                                              |(PSPermGenMax,69271552)
                                              |(PSYoungGenCapacityBefore,1183580160)
                                              |(PSYoungGenCapacityAfter,1183580160)
                                              |(edenBefore,0)
                                              |(edenAfter,0)
                                              |(edenCapacityBefore,922615808)
                                              |(edenCapacityAfter,922615808)
                                              |(fromBefore,29)
                                              |(fromAfter,0)
                                              |(fromCapacityBefore,260964352)
                                              |(fromCapacityAfter,260964352)
                                              |(toBefore,0)
                                              |(toAfter,0)
                                              |(toCapacityBefore,248053760)
                                              |(toCapacityAfter,248053760)
                                              |(ParOldGenCapacityBefore,2863333376)
                                              |(ParOldGenCapacityAfter,2863333376)
                                              |(PSPermGenCapacityBefore,69271552)
                                              |(PSPermGenCapacityAfter,69271552)""".stripMargin
  }

  "CMS heap statistics" should "be parseable" in {
    val events = Parser.parseWithHeapStats(testInput("cms-fragment.txt"))
    events.size shouldBe 1
    events(0).regions.mkString("\n") shouldBe """RegionDelta(par new generation,838848K,5616K,943680K,943680K)
                                                |RegionDelta(eden,100%,0%,838848K,838848K)
                                                |RegionDelta(from,0%,5%,104832K,104832K)
                                                |RegionDelta(to,0%,0%,104832K,104832K)
                                                |RegionDelta(concurrent mark-sweep generation,0K,0K,64K,64K)
                                                |RegionDelta(Metaspace,12441K,12441K,12616K,12616K)
                                                |RegionDelta(class space,1488K,1488K,1562K,1562K)""".stripMargin
  }

  "Incremental parse" should "prompt for more lines" in {
    val lines = """29.538: [GC (Allocation Failure) 29.538: [ParNew
      |Desired survivor size 53673984 bytes, new threshold 1 (max 6)
      |- age   1:   93762072 bytes,   93762072 total
      |: 907401K->104832K(943680K), 0.3764651 secs] 907401K->176460K(1017644K), 0.3765675 secs] [Times: user=0.91 sys=0.19, real=0.38 secs]
      |""".stripMargin.split("\n")

    incrementalParse(lines.take(1).mkString("\n")) shouldBe NeedAnotherLine
    incrementalParse(lines.take(2).mkString("\n")) shouldBe NeedAnotherLine
    incrementalParse(lines.take(3).mkString("\n")) shouldBe NeedAnotherLine
    incrementalParse(lines.take(4).mkString("\n")) shouldBe a [GcEventParsed]
  }

  "Incremental parse" should "ignore unparseable lines" in {
    val lines = """Total time for which application threads were stopped: 0.0132040 seconds
                   |2015-12-04T16:07:12.422+1100: 6994.482: [Full GC [PSYoungGen: 14194K->0K(1376448K)] [ParOldGen: 2788303K->1802287K(2796224K)] 2802498K->1802287K(4172672K) [PSPermGen: 66560K->66131K(132736K)], 3.8232380 secs] [Times: user=10.81 sys=0.06, real=3.83 secs]
                   |""".stripMargin.split("\n")

    incrementalParse(lines.take(1).mkString("\n")) shouldBe SkipLine
    incrementalParse(lines.slice(1, 2).mkString("\n")) shouldBe a [GcEventParsed]
  }

  "Incremental parse" should "ignore unparseable lines in face of matching prefix" in {
    val lines = """28922.782: Total time for which application threads were stopped: 2.9031668 seconds, Stopping threads took: 0.0010975 seconds
                   |28930.272: [Full GC (Allocation Failure) 28930.272: [CMS: 819200K->819199K(819200K), 3.1445149 secs] 1762880K->1289710K(1762880K), [Metaspace: 21984K->21984K(1069056K)], 3.1446281 secs] [Times: user=3.14 sys=0.00, real=3.14 secs]
                   |""".stripMargin.split("\n")

    incrementalParse(lines.take(1).mkString("\n")) shouldBe SkipLine
    incrementalParse(lines.slice(1, 2).mkString("\n")) shouldBe a [GcEventParsed]
  }

  "Incremental parse" should "bump along to heap stats" in {
    val lines = testInput("fragment.txt").split("\n")

    incrementalParse(lines.take(1).mkString("\n")) shouldBe SkipLine
    incrementalParse(lines.slice(1, 2).mkString("\n")) shouldBe NeedAnotherLine
    incrementalParse(lines.slice(1, 22).mkString("\n")) shouldBe NeedAnotherLine
    incrementalParse(lines.slice(1, 23).mkString("\n")) shouldBe a [GcEventParsed]
    incrementalParse(lines.slice(23, 24).mkString("\n")) shouldBe SkipLine
    incrementalParse(lines.slice(33, 34).mkString("\n")) shouldBe a [GcEventParsed]
  }

  "Incremental parse" should "parse heap stats" in {
    val lines = testInput("fragment.txt").split("\n")

    val GcEventParsed(event) = incrementalParse(lines.slice(1, 23).mkString("\n"))
    event.toSeq.mkString("\n") shouldBe """(datetime,2015-12-10 15:46:54.299)
                                          |(age,524176.359)
                                          |(type,GC)
                                          |(pause,0.18402)
                                          |(heapBefore,3748762624)
                                          |(heapAfter,2875555840)
                                          |(heapReclaimed,873206784)
                                          |(heapMax,4046913536)
                                          |(PSYoungGenBefore,1096065024)
                                          |(PSYoungGenAfter,78150656)
                                          |(PSYoungGenReclaimed,1017914368)
                                          |(PSYoungGenMax,1183580160)
                                          |(promoted,144707584)
                                          |(PSYoungGenCapacityBefore,1096089600)
                                          |(PSYoungGenCapacityAfter,1183580160)
                                          |(edenBefore,100)
                                          |(edenAfter,0)
                                          |(edenCapacityBefore,932118528)
                                          |(edenCapacityAfter,922615808)
                                          |(fromBefore,99)
                                          |(fromAfter,29)
                                          |(fromCapacityBefore,163971072)
                                          |(fromCapacityAfter,260964352)
                                          |(toBefore,0)
                                          |(toAfter,0)
                                          |(toCapacityBefore,260964352)
                                          |(toCapacityAfter,248053760)
                                          |(ParOldGenBefore,2652696576)
                                          |(ParOldGenAfter,2797405184)
                                          |(ParOldGenCapacityBefore,2863333376)
                                          |(ParOldGenCapacityAfter,2863333376)
                                          |(PSPermGenBefore,69223424)
                                          |(PSPermGenAfter,69223424)
                                          |(PSPermGenCapacityBefore,69271552)
                                          |(PSPermGenCapacityAfter,69271552)""".stripMargin
  }

  "Incremental parse" should "parse CMS allocation failure" in {
    val line = """28892.707: [GC (Allocation Failure) 28892.707: [ParNew: 943680K->943680K(943680K), 0.0000217 secs]28892.708: [CMS: 745269K->789117K(819200K), 2.5351516 secs] 1688949K->789117K(1762880K), [Metaspace: 21984K->21984K(1069056K)], 2.5433119 secs] [Times: user=2.23 sys=0.01, real=2.54 secs]"""
    val GcEventParsed(event: BasicGCEvent) = incrementalParse(line)
    event.generationDeltas should contain(GenerationDelta("CMS", SizeDelta("745269K", "789117K", "819200K")))
  }

  "Incremental parse" should "parse another breed of parse CMS allocation failure" in {
    val line = """28960.533: [Full GC (Allocation Failure) 28960.533: [CMS: 819199K->819199K(819200K), 3.5242634 secs] 1762879K->1538758K(1762880K), [Metaspace: 21984K->21984K(1069056K)], 3.5243652 secs] [Times: user=3.52 sys=0.00, real=3.52 secs] """
    val GcEventParsed(event: BasicGCEvent) = incrementalParse(line)
    event.generationDeltas should contain(GenerationDelta("CMS", SizeDelta("819199K", "819199K", "819200K")))
  }

//  "Incremental parse" should "parse borked CMS collection" in {
//    val line = """28925.427: [Full GC (Allocation Failure) 28925.427: [CMS28925.436: [CMS-concurrent-sweep: 0.336/0.372 secs] [Times: user=1.08 sys=0.08, real=0.37 secs]
//                 | (concurrent mode failure): 819187K->819199K(819200K), 3.0244510 secs] 1762867K->1215634K(1762880K), [Metaspace: 21984K->21984K(1069056K)], 3.0245827 secs] [Times: user=3.03 sys=0.00, real=3.02 secs]
//                 |""".stripMargin
//    val GcEventParsed(event: BasicGCEvent) = incrementalParse(line)
//    event.generationDeltas should contain(GenerationDelta("CMS", SizeDelta("819199K", "819199K", "819200K")))
//  }

  // examples from https://plumbr.eu/handbook/garbage-collection-algorithms-implementations

  "Plumber.eu –XX:+PrintGCDetails -XX:+PrintGCDateStamps -XX:+PrintGCTimeStamps example" should "parse" in {
    val input = """2015-05-26T14:45:37.987-0200: 151.126: [GC (Allocation Failure) 151.126: [DefNew: 629119K->69888K(629120K), 0.0584157 secs] 1619346K->1273247K(2027264K), 0.0585007 secs] [Times: user=0.06 sys=0.00, real=0.06 secs]
                  |2015-05-26T14:45:59.690-0200: 172.829: [GC (Allocation Failure) 172.829: [DefNew: 629120K->629120K(629120K), 0.0000372 secs]172.829: [Tenured: 1203359K->755802K(1398144K), 0.1855567 secs] 1832479K->755802K(2027264K), [Metaspace: 6741K->6741K(1056768K)], 0.1856954 secs] [Times: user=0.18 sys=0.00, real=0.18 secs]""".stripMargin

    val events = parseLog[BasicGCEvent](input)

    events(0).jvmAgeSeconds shouldBe 151.126
    events(0).pauseSeconds shouldBe 0.0585007
    events(0).heapDelta shouldBe SizeDelta("1619346K", "1273247K", "2027264K")
    events(0).generationDeltas.size shouldBe 1

    events(1).jvmAgeSeconds shouldBe 172.829
    events(1).pauseSeconds shouldBe 0.1856954
    events(1).heapDelta shouldBe SizeDelta("1832479K", "755802K", "2027264K")
    events(1).generationDeltas.map(_.name) shouldBe Seq("DefNew", "Tenured", "Metaspace")

    events.size shouldBe 2
  }

  "Plumber.eu -XX:+UseParallelGC -XX:+UseParallelOldGC example" should "parse"in {
    val input = """2015-05-26T14:27:40.915-0200: 116.115: [GC (Allocation Failure) [PSYoungGen: 2694440K->1305132K(2796544K)] 9556775K->8438926K(11185152K), 0.2406675 secs] [Times: user=1.77 sys=0.01, real=0.24 secs]
                  |2015-05-26T14:27:41.155-0200: 116.356: [Full GC (Ergonomics) [PSYoungGen: 1305132K->0K(2796544K)] [ParOldGen: 7133794K->6597672K(8388608K)] 8438926K->6597672K(11185152K), [Metaspace: 6745K->6745K(1056768K)], 0.9158801 secs] [Times: user=4.49 sys=0.64, real=0.92 secs]""".stripMargin

    val events = parseLog[BasicGCEvent](input)

    events(0).gcType shouldBe "GC"
    events(0).gcCause shouldBe "Allocation Failure"
    events(0).pauseSeconds shouldBe 0.2406675
    events(0).promotedBytes shouldBe Some(271459 * 1024)

    events(1).gcType shouldBe "Full GC"
    events(1).gcCause shouldBe "Ergonomics"
    events(1).generationDeltas.map(_.name) shouldBe Seq("PSYoungGen", "ParOldGen", "Metaspace")
    events(1).pauseSeconds shouldBe 0.9158801

    events.size shouldBe 2
  }

  "CMS initial mark" should "be parsed" in {
    val events = parseLog[CmsGcEvent]("2015-05-26T16:23:07.321-0200: 64.425: [GC (CMS Initial Mark) [1 CMS-initial-mark: 10812086K(11901376K)] 10887844K(12514816K), 0.0001997 secs] [Times: user=0.00 sys=0.00, real=0.00 secs]")
    events(0).gcType shouldBe "GC"
    events(0).gcCause shouldBe "CMS Initial Mark"
    events(0).pauseSeconds shouldBe 0.0
  }

  "CMS-concurrent-mark-start" should "be parsed" in {
    val events = parseLog[CmsGcEvent]("2015-05-26T16:23:07.321-0200: 64.425: [CMS-concurrent-mark-start]")
    events(0).gcType shouldBe "CMS-concurrent-mark-start"
  }

  "CMS-concurrent-mark" should "be parsed" in {
    val events = parseLog[CmsGcEvent]("2015-05-26T16:23:07.357-0200: 64.460: [CMS-concurrent-mark: 0.035/0.035 secs] [Times: user=0.07 sys=0.00, real=0.03 secs]")
    events(0).gcType shouldBe "CMS-concurrent-mark"
    events(0).pauseSeconds shouldBe 0.03
  }

  "Plumber.eu -XX:+UseConcMarkSweepGC example" should "parse"in {
    val input = """2015-05-26T16:23:07.321-0200: 64.425: [GC (CMS Initial Mark) [1 CMS-initial-mark: 10812086K(11901376K)] 10887844K(12514816K), 0.0001997 secs] [Times: user=0.00 sys=0.00, real=0.00 secs]
                  |2015-05-26T16:23:07.321-0200: 64.425: [CMS-concurrent-mark-start]
                  |2015-05-26T16:23:07.357-0200: 64.460: [CMS-concurrent-mark: 0.035/0.035 secs] [Times: user=0.07 sys=0.00, real=0.03 secs]
                  |2015-05-26T16:23:07.357-0200: 64.460: [CMS-concurrent-preclean-start]
                  |2015-05-26T16:23:07.373-0200: 64.476: [CMS-concurrent-preclean: 0.016/0.016 secs] [Times: user=0.02 sys=0.00, real=0.02 secs]
                  |2015-05-26T16:23:07.373-0200: 64.476: [CMS-concurrent-abortable-preclean-start]
                  |2015-05-26T16:23:08.446-0200: 65.550: [CMS-concurrent-abortable-preclean: 0.167/1.074 secs] [Times: user=0.20 sys=0.00, real=1.07 secs]
                  |2015-05-26T16:23:08.447-0200: 65.550: [GC (CMS Final Remark) [YG occupancy: 387920 K (613440 K)]65.550: [Rescan (parallel) , 0.0085125 secs]65.559: [weak refs processing, 0.0000243 secs]65.559: [class unloading, 0.0013120 secs]65.560: [scrub symbol table, 0.0008345 secs]65.561: [scrub string table, 0.0001759 secs][1 CMS-remark: 10812086K(11901376K)] 11200006K(12514816K), 0.0110730 secs] [Times: user=0.06 sys=0.00, real=0.01 secs]
                  |2015-05-26T16:23:08.458-0200: 65.561: [CMS-concurrent-sweep-start]
                  |2015-05-26T16:23:08.485-0200: 65.588: [CMS-concurrent-sweep: 0.027/0.027 secs] [Times: user=0.03 sys=0.00, real=0.03 secs]
                  |2015-05-26T16:23:08.485-0200: 65.589: [CMS-concurrent-reset-start]
                  |2015-05-26T16:23:08.497-0200: 65.601: [CMS-concurrent-reset: 0.012/0.012 secs] [Times: user=0.01 sys=0.00, real=0.01 secs]""".stripMargin

    val events = parseLog[CmsGcEvent](input)

    events(0).gcType shouldBe "GC"
    events(0).gcCause shouldBe "CMS Initial Mark"
    events(0).pauseSeconds shouldBe 0.00

    events(11).gcType shouldBe "CMS-concurrent-reset"
    events(11).pauseSeconds shouldBe 0.01

    events.size shouldBe 12
  }

  "Java 8 promotion failure" should "parse" in {
    val input = """2016-01-28T16:20:24.390+1100: 94045.178: [GC (Allocation Failure) --[PSYoungGen: 1256250K->1256250K(1256448K)] 4037337K->4052791K(4052992K), 0.4414726 secs] [Times: user=0.51 sys=0.00, real=0.45 secs]"""
    val events = parseLog[BasicGCEvent](input)

    events(0).gcType shouldBe "GC--"
    events(0).gcCause shouldBe "Allocation Failure"
    events(0).pauseSeconds shouldBe 0.4414726
  }

}
