# Redis Algebra

A Redis library for Scala that is built on Scalaz's free monad implementation, designed in the spirit of Swierstra's [Data types a la carte](http://www.staff.science.uu.nl/~swier004/Publications/DataTypesALaCarte.pdf). The library is intended to be used in combination with other free monad based libraries to build rich algebras tailored to the user's needs.

The functions that this library provides correspond to the list of [Redis commands](http://redis.io/commands). In order to run programs that have been written using this library, an interpreter needs to be implemented to handle each of the commands. There is no interpreter included by default; however, one may be made available in a separate repository. Interpreters may be interchanged freely based on the user's desired behaviour. For instance, a non-blocking interpreter that sends commands to Redis may be used in production, while an interpreter that uses an in-memory representation may be used when running tests.

# Install

Releases and snapshots of the Redis Algebra library are published to the [Sonatype OSS Repository Hosting Service](https://oss.sonatype.org). The necessary [SBT Resolvers](http://www.scala-sbt.org/release/docs/Detailed-Topics/Resolvers.html) may be added as follows to your SBT build file.

```scala
resolvers += "Sonatype releases" at "http://oss.sonatype.org/content/repositories/releases/"

resolvers += "Sonatype snapshots" at "http://oss.sonatype.org/content/repositories/snapshots/"
```

# Usage

```scala
import scala.language.implicitConversions
import scalaz.{CharSet, NonEmptyList}, NonEmptyList.nels
import scalaz.std.list._
import scalaz.syntax.{Ops, monad, traverse}, monad._, traverse._

import redis.algebra.{F, R}
import redis.algebra.all._

val e0 =
  set[R]("key".utf8, "value".utf8) >>
  get[R]("key".utf8)

val e1 =
  set[R]("counter".utf8, 100L.utf8) >>
  incr[R]("counter".utf8) >>
  incr[R]("counter".utf8) >>
  incrby[R]("counter".utf8, 10L)

val e2 =
  List("first".utf8, "second".utf8, "third".utf8).map(a => rpush[R]("messages".utf8, nels(a))).sequenceU >>
  lrange[R]("messages".utf8, 0, 2)

implicit def StringToStringOps(a: String): StringOps = new StringOps { val self = a }

implicit def LongToStringOps(a: Long): StringOps = new StringOps { val self = a.toString }

sealed abstract class StringOps extends Ops[String] { final def utf8 = self.getBytes(CharSet.UTF8).toIndexedSeq }
```
