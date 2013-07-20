package redis
package algebra

import scalaz.{Free, Functor}, Free.{Gosub, Return, Suspend}

import KeyAlgebra._

trait KeyAlgebra[A]

final case class Del[A](keys: Seq[String], h: Int => A) extends KeyAlgebra[A]

final case class Dump[A](key: String, h: Option[String] => A) extends KeyAlgebra[A]

final case class Exists[A](key: String, h: Boolean => A) extends KeyAlgebra[A]

final case class Expire[A](key: String, in: Seconds, h: Boolean => A) extends KeyAlgebra[A]

final case class ExpireAt[A](key: String, at: Seconds, h: Boolean => A) extends KeyAlgebra[A]

final case class Keys[A](pattern: Glob, h: Seq[String] => A) extends KeyAlgebra[A]

final case class Persist[A](key: String, h: Boolean => A) extends KeyAlgebra[A]

final case class PExpire[A](key: String, in: Milliseconds, h: Boolean => A) extends KeyAlgebra[A]

final case class PExpireAt[A](key: String, at: Milliseconds, h: Boolean => A) extends KeyAlgebra[A]

final case class PTtl[A](key: String, h: Option[Milliseconds] => A) extends KeyAlgebra[A]

final case class RandomKey[A](h: Option[String] => A) extends KeyAlgebra[A]

final case class Rename[A](key: String, name: String, a: A) extends KeyAlgebra[A]

final case class RenameNx[A](key: String, name: String, h: Boolean => A) extends KeyAlgebra[A]

final case class Restore[A](key: String, ttl: Option[Milliseconds], value: String, a: A) extends KeyAlgebra[A]

final case class Ttl[A](key: String, h: Option[Seconds] => A) extends KeyAlgebra[A]

final case class Type[A](key: String, h: Types => A) extends KeyAlgebra[A]

sealed trait Types
case object string extends Types
case object list extends Types
case object set extends Types
case object zset extends Types
case object hash extends Types

sealed trait KeyInstances {
  implicit def keyAlgebraFunctor: Functor[KeyAlgebra] =
    new Functor[KeyAlgebra] {
      def map[A, B](a: KeyAlgebra[A])(f: A => B): KeyAlgebra[B] = a match {
        case Del(k, h) => Del(k, x => f(h(x)))
        case Dump(k, h) => Dump(k, x => f(h(x)))
        case Exists(k, h) => Exists(k, x => f(h(x)))
        case Expire(k, i, h) => Expire(k, i, x => f(h(x)))
        case ExpireAt(k, t, h) => ExpireAt(k, t, x => f(h(x)))
        case Keys(k, h) => Keys(k, x => f(h(x)))
        case Persist(k, h) => Persist(k, x => f(h(x)))
        case PExpire(k, i, h) => PExpire(k, i, x => f(h(x)))
        case PExpireAt(k, t, h) => PExpireAt(k, t, x => f(h(x)))
        case PTtl(k, h) => PTtl(k, x => f(h(x)))
        case RandomKey(h) => RandomKey(x => f(h(x)))
        case Rename(k, n, a) => Rename(k, n, f(a))
        case RenameNx(k, n, h) => RenameNx(k, n, x => f(h(x)))
        case Restore(k, t, v, a) => Restore(k, t, v, f(a))
        case Ttl(k, h) => Ttl(k, x => f(h(x)))
        case Type(k, h) => Type(k, x => f(h(x)))
      }
    }
}

sealed trait KeyFunctions {
  def del(keys: Seq[String]): Free[KeyAlgebra, Int] =
    Suspend[KeyAlgebra, Int](Del(keys, Return(_)))

  def dump(key: String): Free[KeyAlgebra, Option[String]] =
    Suspend[KeyAlgebra, Option[String]](Dump(key, Return(_)))

  def exists(key: String): Free[KeyAlgebra, Boolean] =
    Suspend[KeyAlgebra, Boolean](Exists(key, Return(_)))

  def expire(key: String, in: Seconds): Free[KeyAlgebra, Boolean] =
    Suspend[KeyAlgebra, Boolean](Expire(key, in, Return(_)))

  def expireAt(key: String, at: Seconds): Free[KeyAlgebra, Boolean] =
    Suspend[KeyAlgebra, Boolean](ExpireAt(key, at, Return(_)))

  def keys(pattern: Glob): Free[KeyAlgebra, Seq[String]] =
    Suspend[KeyAlgebra, Seq[String]](Keys(pattern, Return(_)))

  def persist(key: String): Free[KeyAlgebra, Boolean] =
    Suspend[KeyAlgebra, Boolean](Persist(key, Return(_)))

  def pexpire(key: String, in: Milliseconds): Free[KeyAlgebra, Boolean] =
    Suspend[KeyAlgebra, Boolean](PExpire(key, in, Return(_)))

  def pexpireAt(key: String, at: Milliseconds): Free[KeyAlgebra, Boolean] =
    Suspend[KeyAlgebra, Boolean](PExpireAt(key, at, Return(_)))

  def pttl(key: String): Free[KeyAlgebra, Option[Milliseconds]] =
    Suspend[KeyAlgebra, Option[Milliseconds]](PTtl(key, Return(_)))

  def randomKey: Free[KeyAlgebra, Option[String]] =
    Suspend[KeyAlgebra, Option[String]](RandomKey(Return(_)))

  def rename(key: String, name: String): Free[KeyAlgebra, Unit] =
    Suspend[KeyAlgebra, Unit](Rename(key, name, Return(())))

  def renameNx(key: String, name: String): Free[KeyAlgebra, Boolean] =
    Suspend[KeyAlgebra, Boolean](RenameNx(key, name, Return(_)))

  def restore(key: String, value: String, ttl: Option[Milliseconds] = None): Free[KeyAlgebra, Unit] =
    Suspend[KeyAlgebra, Unit](Restore(key, ttl, value, Return(())))

  def ttl(key: String): Free[KeyAlgebra, Option[Seconds]] =
    Suspend[KeyAlgebra, Option[Seconds]](Ttl(key, Return(_)))

  def type_(key: String): Free[KeyAlgebra, Types] =
    Suspend[KeyAlgebra, Types](Type(key, Return(_)))
}

object KeyAlgebra extends KeyInstances with KeyFunctions
