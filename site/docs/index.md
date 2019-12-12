---
layout: home

---

# gato-parsec - Parsers for Generic Types [![Build Status](https://travis-ci.com/ChristopherDavenport/gato-parsec.svg?branch=master)](https://travis-ci.com/ChristopherDavenport/gato-parsec) [![Maven Central](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/gato-parsec_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/io.chrisdavenport/gato-parsec_2.12)

## Quick Start

To use gato-parsec in an existing SBT project with Scala 2.11 or a later version, add the following dependencies to your
`build.sbt` depending on your needs:

```scala
libraryDependencies ++= Seq(
  "io.chrisdavenport" %% "gato-parsec" % "<version>"
)
```

## Performance

At this point, gato-parsec is an experimental library. While [some effort](https://github.com/ChristopherDavenport/gato-parsec/pull/5) has been put into making it reasonably performant, if performance is really critical to your application, gato-parsec in its current form might not be the library for you.

Be aware that in the worst case, gen-parsec could have a memory requirement on par with *2x* the size of the input data. In practice, [structural sharing](https://en.wikipedia.org/wiki/Persistent_data_structure) should usually result in significantly lower actual memory usage.
