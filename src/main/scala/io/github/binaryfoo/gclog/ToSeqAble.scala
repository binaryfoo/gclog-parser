package io.github.binaryfoo.gclog

trait ToSeqAble {
  def toSeq: Seq[(String, String)]
}
