package io.github.binaryfoo.gclog

import fastparse.all._
import org.scalatest.{FlatSpec, Matchers}

class EndsWithCurlyBracketTest extends FlatSpec with Matchers {

  "abc}" should "be matched" in {
    val Parsed.Success(value, _) = EndsWithCurlyBracket.!.parse("abc}")
    value shouldBe "abc}"
  }

  "}" should "be matched" in {
    val Parsed.Success(value, _) = EndsWithCurlyBracket.!.parse("}")
    value shouldBe "}"
  }

  "{abc" should "not be matched" in {
    val Parsed.Failure(_, index, _) = EndsWithCurlyBracket.!.parse("{abc")
    index shouldBe 3
  }

  "}a" should "not be matched" in {
    val Parsed.Failure(_, index, _) = EndsWithCurlyBracket.!.parse("}a")
    index shouldBe 1
  }
}
