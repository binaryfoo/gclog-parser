package io.github.binaryfoo.gclog

import fastparse.all._
import org.scalatest.{FlatSpec, Matchers}

class DigitsTest extends FlatSpec with Matchers {

  "3 digits" should "be matched" in {
    val Parsed.Success("123", _) = Digits(3).!.parse("123")
    val Parsed.Failure(_, 2, _) = Digits(3).!.parse("12")
  }

  "At least 1 digit" should "be matched" in {
    val Parsed.Success("123", _) = AtLeastDigits(1).!.parse("123")
    val Parsed.Failure(_, 0, _) = AtLeastDigits(1).!.parse("")
  }

}
