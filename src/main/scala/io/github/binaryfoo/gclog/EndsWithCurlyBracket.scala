package io.github.binaryfoo.gclog

import fastparse.core.ParseCtx

object EndsWithCurlyBracket extends fastparse.core.Parser[Unit] {

  def parseRec(cfg: ParseCtx, index: Int) = {
    val input = cfg.input
    val last = input.length - 1
    if (input(last) != '}') fail(cfg.failure, last)
    else success(cfg.success, (), last + 1, Nil, false)
  }
}

object Digits {
  def isDigit(c: Char) = {
    val i: Int = c
    48 <= i && i <= 57
  }
}

case class Digits(n: Int) extends fastparse.core.Parser[Unit] {
  def parseRec(cfg: ParseCtx, index: Int) = {
    var curr = index
    val input = cfg.input
    while(curr < input.length && Digits.isDigit(input(curr))) curr += 1
    if (curr - index != n) fail(cfg.failure, curr)
    else success(cfg.success, (), curr, Nil, false)
  }
}

case class AtLeastDigits(n: Int) extends fastparse.core.Parser[Unit] {
  def parseRec(cfg: ParseCtx, index: Int) = {
    var curr = index
    val input = cfg.input
    while(curr < input.length && Digits.isDigit(input(curr))) curr += 1
    if (curr - index < n) fail(cfg.failure, curr)
    else success(cfg.success, (), curr, Nil, false)
  }
}