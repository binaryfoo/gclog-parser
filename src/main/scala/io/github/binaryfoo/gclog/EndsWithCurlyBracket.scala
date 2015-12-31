package io.github.binaryfoo.gclog

import fastparse.core.ParseCtx

object EndsWithCurlyBracket extends fastparse.core.Parser[Unit]{

  def parseRec(cfg: ParseCtx, index: Int) = {
    val input = cfg.input
    val last = input.length - 1
    if (input(last) != '}') fail(cfg.failure, last)
    else success(cfg.success, (), last + 1, Nil, false)
  }
}
