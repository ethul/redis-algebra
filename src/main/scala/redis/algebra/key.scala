package redis
package algebra

import scalaz.{\/, Free, Functor, NonEmptyList}, Free.{Gosub, Return, Suspend}

import typeclass.Inject, Inject._

import KeyAlgebra._

sealed trait KeyAlgebra[A]

final case class Del[A](keys: NonEmptyList[String], h: Long => A) extends KeyAlgebra[A]

final case class Dump[A](key: String, h: Option[String] => A) extends KeyAlgebra[A]

final case class Exists[A](key: String, h: Boolean => A) extends KeyAlgebra[A]

final case class Expire[A](key: String, in: Seconds, h: Boolean => A) extends KeyAlgebra[A]

final case class Expireat[A](key: String, at: Seconds, h: Boolean => A) extends KeyAlgebra[A]

final case class Keys[A](pattern: Glob, h: Seq[String] => A) extends KeyAlgebra[A]

final case class Migrate[A](host: String, port: Int, key: String, timeout: Milliseconds, destination: Short, copy: Boolean, replace: Boolean, h: Boolean => A) extends KeyAlgebra[A]

final case class Move[A](key: String, db: Short, h: Boolean => A) extends KeyAlgebra[A]

final case class Object[A](subcommand: Subcommand, h: String \/ Long => A) extends KeyAlgebra[A]

final case class Persist[A](key: String, h: Boolean => A) extends KeyAlgebra[A]

final case class Pexpire[A](key: String, in: Milliseconds, h: Boolean => A) extends KeyAlgebra[A]

final case class Pexpireat[A](key: String, at: Milliseconds, h: Boolean => A) extends KeyAlgebra[A]

final case class Pttl[A](key: String, h: Option[Milliseconds] => A) extends KeyAlgebra[A]

final case class Randomkey[A](h: Option[String] => A) extends KeyAlgebra[A]

final case class Rename[A](key: String, name: String, a: A) extends KeyAlgebra[A]

final case class Renamenx[A](key: String, name: String, h: Boolean => A) extends KeyAlgebra[A]

final case class Restore[A](key: String, ttl: Option[Milliseconds], value: String, a: A) extends KeyAlgebra[A]

final case class Sort[A](key: String, by: Option[By], limit: Option[Limit], get: Seq[Glob], order: Order, alpha: Boolean, store: Option[String], h: Seq[String] => A) extends KeyAlgebra[A]

final case class Ttl[A](key: String, h: Option[Seconds] => A) extends KeyAlgebra[A]

final case class Type[A](key: String, h: Types => A) extends KeyAlgebra[A]

sealed trait Types
case object string_ extends Types
case object list_ extends Types
case object set_ extends Types
case object zset_ extends Types
case object hash_ extends Types

sealed trait Subcommand
final case class refcount(key: String) extends Subcommand
final case class encoding(key: String) extends Subcommand
final case class idletime(key: String) extends Subcommand

sealed trait By
case object nosort extends By
case class pattern(pattern: Glob) extends By

sealed trait Order
case object asc extends Order
case object desc extends Order

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
        case Migrate(o, p, k, t, d, c, r, h) => Migrate(o, p, k, t, d, c, r, x => f(h(x)))
        case Move(k, d, h) => Move(k, d, x => f(h(x)))
        case Object(s, h) => Object(s, x => f(h(x)))
        case Persist(k, h) => Persist(k, x => f(h(x)))
        case Pexpire(k, i, h) => Pexpire(k, i, x => f(h(x)))
        case Pexpireat(k, t, h) => Pexpireat(k, t, x => f(h(x)))
        case Pttl(k, h) => Pttl(k, x => f(h(x)))
        case Randomkey(h) => Randomkey(x => f(h(x)))
        case Rename(k, n, a) => Rename(k, n, f(a))
        case Renamenx(k, n, h) => Renamenx(k, n, x => f(h(x)))
        case Restore(k, t, v, a) => Restore(k, t, v, f(a))
        case Sort(k, b, l, g, o, a, s, h) => Sort(k, b, l, g, o, a, s, x => f(h(x)))
        case Ttl(k, h) => Ttl(k, x => f(h(x)))
        case Type(k, h) => Type(k, x => f(h(x)))
      }
    }
}

sealed trait KeyFunctions {
  def del[F[_]: Functor](keys: NonEmptyList[String])(implicit I: Inject[KeyAlgebra, F]): Free[F, Long] =
    inject[F, KeyAlgebra, Long](Del(keys, Return(_)))

  def dump[F[_]: Functor](key: String)(implicit I: Inject[KeyAlgebra, F]): Free[F, Option[String]] =
    inject[F, KeyAlgebra, Option[String]](Dump(key, Return(_)))

  def exists[F[_]: Functor](key: String)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Exists(key, Return(_)))

  def expire[F[_]: Functor](key: String, in: Seconds)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Expire(key, in, Return(_)))

  def expireat[F[_]: Functor](key: String, at: Seconds)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Expireat(key, at, Return(_)))

  def keys[F[_]: Functor](pattern: Glob)(implicit I: Inject[KeyAlgebra, F]): Free[F, Seq[String]] =
    inject[F, KeyAlgebra, Seq[String]](Keys(pattern, Return(_)))

  def migrate[F[_]: Functor](
    host: String,
    port: Int,
    key: String,
    timeout: Milliseconds,
    destination: Short,
    copy: Boolean = false,
    replace: Boolean = false)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Migrate(host, port, key, timeout, destination, copy, replace, Return(_)))

  def move[F[_]: Functor](key: String, db: Short)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Move(key, db, Return(_)))

  def `object`[F[_]: Functor](subcommand: Subcommand)(implicit I: Inject[KeyAlgebra, F]): Free[F, String \/ Long] =
    inject[F, KeyAlgebra, String \/ Long](Object(subcommand, Return(_)))

  def persist[F[_]: Functor](key: String)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Persist(key, Return(_)))

  def pexpire[F[_]: Functor](key: String, in: Milliseconds)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Pexpire(key, in, Return(_)))

  def pexpireat[F[_]: Functor](key: String, at: Milliseconds)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Pexpireat(key, at, Return(_)))

  def pttl[F[_]: Functor](key: String)(implicit I: Inject[KeyAlgebra, F]): Free[F, Option[Milliseconds]] =
    inject[F, KeyAlgebra, Option[Milliseconds]](Pttl(key, Return(_)))

  def randomkey[F[_]: Functor](implicit I: Inject[KeyAlgebra, F]): Free[F, Option[String]] =
    inject[F, KeyAlgebra, Option[String]](Randomkey(Return(_)))

  def rename[F[_]: Functor](key: String, name: String)(implicit I: Inject[KeyAlgebra, F]): Free[F, Unit] =
    inject[F, KeyAlgebra, Unit](Rename(key, name, Return(())))

  def renamenx[F[_]: Functor](key: String, name: String)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Renamenx(key, name, Return(_)))

  def restore[F[_]: Functor](key: String, value: String, ttl: Option[Milliseconds] = None)(implicit I: Inject[KeyAlgebra, F]): Free[F, Unit] =
    inject[F, KeyAlgebra, Unit](Restore(key, ttl, value, Return(())))

  def sort[F[_]: Functor](
    key: String,
    by: Option[By] = None,
    limit: Option[Limit] = None,
    get: Seq[Glob] = Nil,
    order: Order = asc,
    alpha: Boolean = false,
    store: Option[String] = None)(implicit I: Inject[KeyAlgebra, F]): Free[F, Seq[String]] =
    inject[F, KeyAlgebra, Seq[String]](Sort(key, by, limit, get, order, alpha, store, Return(_)))

  def ttl[F[_]: Functor](key: String)(implicit I: Inject[KeyAlgebra, F]): Free[F, Option[Seconds]] =
    inject[F, KeyAlgebra, Option[Seconds]](Ttl(key, Return(_)))

  def `type`[F[_]: Functor](key: String)(implicit I: Inject[KeyAlgebra, F]): Free[F, Types] =
    inject[F, KeyAlgebra, Types](Type(key, Return(_)))
}

object KeyAlgebra extends KeyInstances with KeyFunctions
