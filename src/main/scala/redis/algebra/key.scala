package redis
package algebra

import scalaz.{Free, Functor, NonEmptyList}, Free.{Gosub, Return, Suspend}

import KeyAlgebra._

sealed trait KeyAlgebra[A] extends RedisAlgebra[A]

final case class Del[A](keys: NonEmptyList[String], h: Int => A) extends KeyAlgebra[A]

final case class Dump[A](key: String, h: Option[String] => A) extends KeyAlgebra[A]

final case class Exists[A](key: String, h: Boolean => A) extends KeyAlgebra[A]

final case class Expire[A](key: String, in: Seconds, h: Boolean => A) extends KeyAlgebra[A]

final case class Expireat[A](key: String, at: Seconds, h: Boolean => A) extends KeyAlgebra[A]

final case class Keys[A](pattern: Glob, h: Seq[String] => A) extends KeyAlgebra[A]

final case class Persist[A](key: String, h: Boolean => A) extends KeyAlgebra[A]

final case class Pexpire[A](key: String, in: Milliseconds, h: Boolean => A) extends KeyAlgebra[A]

final case class Pexpireat[A](key: String, at: Milliseconds, h: Boolean => A) extends KeyAlgebra[A]

final case class Pttl[A](key: String, h: Option[Milliseconds] => A) extends KeyAlgebra[A]

final case class Randomkey[A](h: Option[String] => A) extends KeyAlgebra[A]

final case class Rename[A](key: String, name: String, a: A) extends KeyAlgebra[A]

final case class Renamenx[A](key: String, name: String, h: Boolean => A) extends KeyAlgebra[A]

final case class Restore[A](key: String, ttl: Option[Milliseconds], value: String, a: A) extends KeyAlgebra[A]

final case class Ttl[A](key: String, h: Option[Seconds] => A) extends KeyAlgebra[A]

final case class Type[A](key: String, h: Types => A) extends KeyAlgebra[A]

sealed trait Types
case object string_ extends Types
case object list_ extends Types
case object set_ extends Types
case object zset_ extends Types
case object hash_ extends Types

sealed trait KeyInstances {
  implicit def keyAlgebraFunctor: Functor[KeyAlgebra] =
    new Functor[KeyAlgebra] {
      def map[A, B](a: KeyAlgebra[A])(f: A => B): KeyAlgebra[B] = a match {
        case Del(k, h) => Del(k, x => f(h(x)))
        case Dump(k, h) => Dump(k, x => f(h(x)))
        case Exists(k, h) => Exists(k, x => f(h(x)))
        case Expire(k, i, h) => Expire(k, i, x => f(h(x)))
        case Expireat(k, t, h) => Expireat(k, t, x => f(h(x)))
        case Keys(k, h) => Keys(k, x => f(h(x)))
        case Persist(k, h) => Persist(k, x => f(h(x)))
        case Pexpire(k, i, h) => Pexpire(k, i, x => f(h(x)))
        case Pexpireat(k, t, h) => Pexpireat(k, t, x => f(h(x)))
        case Pttl(k, h) => Pttl(k, x => f(h(x)))
        case Randomkey(h) => Randomkey(x => f(h(x)))
        case Rename(k, n, a) => Rename(k, n, f(a))
        case Renamenx(k, n, h) => Renamenx(k, n, x => f(h(x)))
        case Restore(k, t, v, a) => Restore(k, t, v, f(a))
        case Ttl(k, h) => Ttl(k, x => f(h(x)))
        case Type(k, h) => Type(k, x => f(h(x)))
      }
    }
}

sealed trait KeyFunctions {
  def del(keys: NonEmptyList[String]): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Del(keys, Return(_)))

  def dump(key: String): Free[RedisAlgebra, Option[String]] =
    Suspend[RedisAlgebra, Option[String]](Dump(key, Return(_)))

  def exists(key: String): Free[RedisAlgebra, Boolean] =
    Suspend[RedisAlgebra, Boolean](Exists(key, Return(_)))

  def expire(key: String, in: Seconds): Free[RedisAlgebra, Boolean] =
    Suspend[RedisAlgebra, Boolean](Expire(key, in, Return(_)))

  def expireat(key: String, at: Seconds): Free[RedisAlgebra, Boolean] =
    Suspend[RedisAlgebra, Boolean](Expireat(key, at, Return(_)))

  def keys(pattern: Glob): Free[RedisAlgebra, Seq[String]] =
    Suspend[RedisAlgebra, Seq[String]](Keys(pattern, Return(_)))

  def persist(key: String): Free[RedisAlgebra, Boolean] =
    Suspend[RedisAlgebra, Boolean](Persist(key, Return(_)))

  def pexpire(key: String, in: Milliseconds): Free[RedisAlgebra, Boolean] =
    Suspend[RedisAlgebra, Boolean](Pexpire(key, in, Return(_)))

  def pexpireat(key: String, at: Milliseconds): Free[RedisAlgebra, Boolean] =
    Suspend[RedisAlgebra, Boolean](Pexpireat(key, at, Return(_)))

  def pttl(key: String): Free[RedisAlgebra, Option[Milliseconds]] =
    Suspend[RedisAlgebra, Option[Milliseconds]](Pttl(key, Return(_)))

  def randomkey: Free[RedisAlgebra, Option[String]] =
    Suspend[RedisAlgebra, Option[String]](Randomkey(Return(_)))

  def rename(key: String, name: String): Free[RedisAlgebra, Unit] =
    Suspend[RedisAlgebra, Unit](Rename(key, name, Return(())))

  def renamenx(key: String, name: String): Free[RedisAlgebra, Boolean] =
    Suspend[RedisAlgebra, Boolean](Renamenx(key, name, Return(_)))

  def restore(key: String, value: String, ttl: Option[Milliseconds] = None): Free[RedisAlgebra, Unit] =
    Suspend[RedisAlgebra, Unit](Restore(key, ttl, value, Return(())))

  def ttl(key: String): Free[RedisAlgebra, Option[Seconds]] =
    Suspend[RedisAlgebra, Option[Seconds]](Ttl(key, Return(_)))

  def type_(key: String): Free[RedisAlgebra, Types] =
    Suspend[RedisAlgebra, Types](Type(key, Return(_)))
}

object KeyAlgebra extends KeyInstances with KeyFunctions
