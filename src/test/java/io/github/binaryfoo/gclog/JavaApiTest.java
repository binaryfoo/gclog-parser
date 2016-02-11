package io.github.binaryfoo.gclog;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.junit.Test;
import scala.collection.Seq;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;

import static org.hamcrest.core.Is.is;
import static org.junit.Assert.assertThat;

public class JavaApiTest {

  private static DateTimeZone Plus11 = DateTimeZone.forOffsetHours(11);

  @Test
  public void parseLog() throws IOException {
    Seq<BasicGCEvent> events = Parser.parseLog(testInput("fragment.txt"));
    assertThat(events.apply(0).time(), is(new DateTime(2015, 12, 10, 15, 46, 54, 299, Plus11)));
    assertThat(events.apply(0).gcType(), is("GC"));
    assertThat(events.apply(1).time(), is(new DateTime(2015, 12, 10, 15, 46, 54, 493, Plus11)));
    assertThat(events.apply(1).gcType(), is("Full GC"));
  }

  @Test
  public void parseLogAsJavaList() throws IOException {
    List<GCEvent> events = JavaParser.parseLog(testInput("fragment.txt"));
    assertThat(events.get(0).time(), is(new DateTime(2015, 12, 10, 15, 46, 54, 299, Plus11)));
    assertThat(events.get(0).gcType(), is("GC"));
    assertThat(events.get(1).time(), is(new DateTime(2015, 12, 10, 15, 46, 54, 493, Plus11)));
    assertThat(events.get(1).gcType(), is("Full GC"));
  }

  private String testInput(String fileName) throws IOException {
    return new String(Files.readAllBytes(new File("src/test/resources/" + fileName).toPath()));
  }
}
