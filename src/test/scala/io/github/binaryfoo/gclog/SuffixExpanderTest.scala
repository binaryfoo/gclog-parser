package io.github.binaryfoo.gclog

import org.scalatest.{Matchers, FlatSpec}

class SuffixExpanderTest extends FlatSpec with Matchers {

  "Kilobytes" should "be expanded" in {
    SuffixExpander.toBytes("3K") shouldBe 3072
  }
}
