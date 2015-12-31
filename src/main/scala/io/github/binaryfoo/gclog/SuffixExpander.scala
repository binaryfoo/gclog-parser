package io.github.binaryfoo.gclog

object SuffixExpander {
  def toBytes(v: String): Long = {
    val multiplier = v.charAt(v.length - 1) match {
      case 'K' => 1024
      case '%' => 1 // hack
    }
    v.substring(0, v.length - 1).toLong * multiplier
  }
  def expandSuffix(v: String): String = SuffixExpander.toBytes(v).toString
}
