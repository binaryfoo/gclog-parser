package io.github.binaryfoo.gclog;

import scala.collection.JavaConversions;

import java.util.List;

public class JavaParser {
  public static List<GCEvent> parseLog(String log) {
    return JavaConversions.seqAsJavaList(Parser.parseLog(log));
  }
}
