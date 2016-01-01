[ ![Codeship Status for binaryfoo/gclog-parser](https://codeship.com/projects/1bcfe1b0-92a3-0133-bc86-721317efc9bb/status?branch=master)](https://codeship.com/projects/124747)

Parser for java garbage collection logs.

At least the output written by hotspot's -verbose:gc flag.

Use as a library from [maven central](http://search.maven.org/#search%7Cga%7C1%7Cgclog-parser):

    <dependency>
        <groupId>io.github.binaryfoo</groupId>
        <artifactId>gclog-parser</artifactId>
        <version>0.0.2</version>
    </dependency>

Or [Download](http://search.maven.org/remotecontent?filepath=io/github/binaryfoo/gclog-parser/0.0.2/gclog-parser-0.0.2-assembly.jar) as a command line tool to convert a garbage collection log to a .csv file.