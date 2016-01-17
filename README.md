[ ![Codeship Status for binaryfoo/gclog-parser](https://codeship.com/projects/1bcfe1b0-92a3-0133-bc86-721317efc9bb/status?branch=master)](https://codeship.com/projects/124747)

## What?

Parser for java garbage collection logs.

At least the output written by hotspot's -verbose:gc flag.

## Why?

Your JVM died with OutOfMemoryError, ran depresingly slowly or perhaps with unpredictable latency.

The [Java garbage collection handbook](https://plumbr.eu/java-garbage-collection-handbook) from Plumbr
has an excellent write up of the concepts involved.

## How?

Use as a library from [maven central](http://search.maven.org/#search%7Cga%7C1%7Cgclog-parser):

    <dependency>
        <groupId>io.github.binaryfoo</groupId>
        <artifactId>gclog-parser</artifactId>
        <version>0.0.2</version>
    </dependency>

Or [Download](https://oss.sonatype.org/service/local/artifact/maven/content?r=snapshots&g=io.github.binaryfoo&a=gclog-parser&c=assembly&v=LATEST') as a command line tool to convert a garbage collection log to a .csv file.

Or use from [lagotto](https://github.com/binaryfoo/lagotto) to produce [basic charts](http://binaryfoo.github.io/lagotto/#_garbage_collection_logs).