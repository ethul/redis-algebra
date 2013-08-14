# Redis Algebra

A Redis library for Scala that is built on Scalaz's free monad implementation, designed in the spirit of Swierstra's [Data types a la carte](http://www.staff.science.uu.nl/~swier004/Publications/DataTypesALaCarte.pdf). The library is intended to be used in combination with other free monad based libraries to build rich algebras tailored to the user's needs.

The functions that this library provides correspond to the list of [Redis commands](http://redis.io/commands). In order to run programs that have been written using this library, an interpreter needs to be implemented to handle each of the commands. There is no interpreter included by default; however, one may be made available in a separate repository. Interpreters may be interchanged freely based on the user's desired behaviour. For instance, a non-blocking interpreter that sends commands to Redis may be used in production, while an interpreter that uses an in-memory representation may be used when running tests.

# Install

Currently, a snapshot of the Redis Algebra library is available in the following repository. The snippet below may be added to an SBT build file in order to use the Redis Algebra.

```scala
libraryDependencies += "redis-algebra" %% "redis-algebra" % "0.0.1-SNAPSHOT"

resolvers += "Github ethul/ivy-repository snapshots" at "https://github.com/ethul/ivy-repository/raw/master/snapshots/"
```

# Usage

```scala
import scalaz.NonEmptyList, NonEmptyList._
import scalaz.std.list._
import scalaz.syntax.all._

import redis.algebra.{F, R}
import redis.algebra.{KeyAlgebra, ListAlgebra, StringAlgebra}, KeyAlgebra._, ListAlgebra._, StringAlgebra._

val e0 =
  set[R]("key", "value") >>
  get[R]("key")

val e1 =
  set[R]("counter", "100") >>
  incr[R]("counter") >>
  incr[R]("counter") >>
  incrby[R]("counter", 10)

val e2 =
  List("first", "second", "third").map(a => rpush[R]("messages", nels(a))).sequenceU >>
  lrange[R]("messages", 0, 2)
```
