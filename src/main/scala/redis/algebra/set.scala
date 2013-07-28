package redis
package algebra

import scalaz.{Free, Functor, NonEmptyList}, Free.{Gosub, Return, Suspend}

import SetAlgebra._

sealed trait SetAlgebra[A] extends RedisAlgebra[A]

final case class Sadd[A](key: String, members: NonEmptyList[String], h: Int => A) extends SetAlgebra[A]

final case class Scard[A](key: String, h: Int => A) extends SetAlgebra[A]

final case class Sdiff[A](keys: NonEmptyList[String], h: Set[String] => A) extends SetAlgebra[A]

final case class Sdiffstore[A](destination: String, keys: NonEmptyList[String], h: Int => A) extends SetAlgebra[A]

final case class Sinter[A](keys: NonEmptyList[String], h: Set[String] => A) extends SetAlgebra[A]

final case class Sinterstore[A](destination: String, keys: NonEmptyList[String], h: Int => A) extends SetAlgebra[A]

final case class Sismember[A](key: String, member: String, h: Boolean => A) extends SetAlgebra[A]

final case class Smembers[A](key: String, h: Set[String] => A) extends SetAlgebra[A]

final case class Smove[A](source: String, destination: String, member: String, h: Boolean => A) extends SetAlgebra[A]

final case class Spop[A](key: String, h: Option[String] => A) extends SetAlgebra[A]

final case class Srandmember[A](key: String, count: Option[Int], h: Set[String] => A) extends SetAlgebra[A]

final case class Srem[A](key: String, members: NonEmptyList[String], h: Int => A) extends SetAlgebra[A]

final case class Sunion[A](keys: NonEmptyList[String], h: Set[String] => A) extends SetAlgebra[A]

final case class Sunionstore[A](destination: String, keys: NonEmptyList[String], h: Int => A) extends SetAlgebra[A]

sealed trait SetInstances {
  implicit def setAlgebraFunctor: Functor[SetAlgebra] =
    new Functor[SetAlgebra] {
      def map[A, B](a: SetAlgebra[A])(f: A => B): SetAlgebra[B] = a match {
        case Sadd(k, m, h) => Sadd(k, m, x => f(h(x)))
        case Scard(k, h) => Scard(k, x => f(h(x)))
        case Sdiff(k, h) => Sdiff(k, x => f(h(x)))
        case Sdiffstore(d, k, h) => Sdiffstore(d, k, x => f(h(x)))
        case Sinter(k, h) => Sinter(k, x => f(h(x)))
        case Sinterstore(d, k, h) => Sinterstore(d, k, x => f(h(x)))
        case Sismember(k, m, h) => Sismember(k, m, x => f(h(x)))
        case Smembers(k, h) => Smembers(k, x => f(h(x)))
        case Smove(s, d, m, h) => Smove(s, d, m, x => f(h(x)))
        case Spop(k, h) => Spop(k, x => f(h(x)))
        case Srandmember(k, c, h) => Srandmember(k, c, x => f(h(x)))
        case Srem(k, m, h) => Srem(k, m, x => f(h(x)))
        case Sunion(k, h) => Sunion(k, x => f(h(x)))
        case Sunionstore(d, k, h) => Sunionstore(d, k, x => f(h(x)))
      }
    }
}

sealed trait SetFunctions {
  def sadd(key: String, members: NonEmptyList[String]): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Sadd(key, members, Return(_)))

  def scard(key: String): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Scard(key, Return(_)))

  def sdiff(keys: NonEmptyList[String]): Free[RedisAlgebra, Set[String]] =
    Suspend[RedisAlgebra, Set[String]](Sdiff(keys, Return(_)))

  def sdiffstore(destination: String, keys: NonEmptyList[String]): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Sdiffstore(destination, keys, Return(_)))

  def sinter(keys: NonEmptyList[String]): Free[RedisAlgebra, Set[String]] =
    Suspend[RedisAlgebra, Set[String]](Sinter(keys, Return(_)))

  def sinterstore(destination: String, keys: NonEmptyList[String]): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Sinterstore(destination, keys, Return(_)))

  def sismember(key: String, member: String): Free[RedisAlgebra, Boolean] =
    Suspend[RedisAlgebra, Boolean](Sismember(key, member, Return(_)))

  def smembers(key: String): Free[RedisAlgebra, Set[String]] =
    Suspend[RedisAlgebra, Set[String]](Smembers(key, Return(_)))

  def smove(source: String, destination: String, member: String): Free[RedisAlgebra, Boolean] =
    Suspend[RedisAlgebra, Boolean](Smove(source, destination, member, Return(_)))

  def spop(key: String): Free[RedisAlgebra, Option[String]] =
    Suspend[RedisAlgebra, Option[String]](Spop(key, Return(_)))

  def srandmember(key: String, count: Option[Int] = None): Free[RedisAlgebra, Set[String]] =
    Suspend[RedisAlgebra, Set[String]](Srandmember(key, count, Return(_)))

  def srem(key: String, members: NonEmptyList[String]): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Srem(key, members, Return(_)))

  def sunion(keys: NonEmptyList[String]): Free[RedisAlgebra, Set[String]] =
    Suspend[RedisAlgebra, Set[String]](Sunion(keys, Return(_)))

  def sunionstore(destination: String, keys: NonEmptyList[String]): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Sunionstore(destination, keys, Return(_)))
}

object SetAlgebra extends SetInstances with SetFunctions
