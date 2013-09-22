package redis
package algebra

import scalaz.{\/, Free, Functor, Inject, InjectFunctions, NonEmptyList}, Free.Return

import data.{Asc, By, Limit, ObjectSubcommand, ObjectResult, Order, Type => DataType, Status}

sealed abstract class KeyAlgebra[A]

final case class Del[A](keys: NonEmptyList[ByteString], h: Long => A) extends KeyAlgebra[A]

final case class Dump[A](key: ByteString, h: Option[ByteString] => A) extends KeyAlgebra[A]

final case class Exists[A](key: ByteString, h: Boolean => A) extends KeyAlgebra[A]

final case class Expire[A](key: ByteString, in: Seconds, h: Boolean => A) extends KeyAlgebra[A]

final case class Expireat[A](key: ByteString, at: Seconds, h: Boolean => A) extends KeyAlgebra[A]

final case class Keys[A](pattern: ByteString, h: Seq[ByteString] => A) extends KeyAlgebra[A]

final case class Migrate[A](host: ByteString, port: Int, key: ByteString, timeout: Milliseconds, destination: Short, copy: Boolean, replace: Boolean, h: Status => A) extends KeyAlgebra[A]

final case class Move[A](key: ByteString, db: Short, h: Boolean => A) extends KeyAlgebra[A]

final case class Object[A](subcommand: ObjectSubcommand, h: Option[ObjectResult] => A) extends KeyAlgebra[A]

final case class Persist[A](key: ByteString, h: Boolean => A) extends KeyAlgebra[A]

final case class Pexpire[A](key: ByteString, in: Milliseconds, h: Boolean => A) extends KeyAlgebra[A]

final case class Pexpireat[A](key: ByteString, at: Milliseconds, h: Boolean => A) extends KeyAlgebra[A]

final case class Pttl[A](key: ByteString, h: Option[Milliseconds] => A) extends KeyAlgebra[A]

final case class Randomkey[A](h: Option[ByteString] => A) extends KeyAlgebra[A]

final case class Rename[A](key: ByteString, name: ByteString, h: Status => A) extends KeyAlgebra[A]

final case class Renamenx[A](key: ByteString, name: ByteString, h: Boolean => A) extends KeyAlgebra[A]

final case class Restore[A](key: ByteString, ttl: Option[Milliseconds], value: ByteString, h: Status => A) extends KeyAlgebra[A]

final case class Sort[A](key: ByteString, by: Option[By], limit: Option[Limit], get: Seq[ByteString], order: Order, alpha: Boolean, store: Option[ByteString], h: Seq[ByteString] \/ Long => A) extends KeyAlgebra[A]

final case class Ttl[A](key: ByteString, h: Option[Seconds] => A) extends KeyAlgebra[A]

final case class Type[A](key: ByteString, h: Option[DataType] => A) extends KeyAlgebra[A]

trait KeyInstances {
  implicit val keyAlgebraFunctor: Functor[KeyAlgebra] =
    new Functor[KeyAlgebra] {
      def map[A, B](a: KeyAlgebra[A])(f: A => B): KeyAlgebra[B] =
        a match {
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
          case Rename(k, n, h) => Rename(k, n, x => f(h(x)))
          case Renamenx(k, n, h) => Renamenx(k, n, x => f(h(x)))
          case Restore(k, t, v, h) => Restore(k, t, v, x => f(h(x)))
          case Sort(k, b, l, g, o, a, s, h) => Sort(k, b, l, g, o, a, s, x => f(h(x)))
          case Ttl(k, h) => Ttl(k, x => f(h(x)))
          case Type(k, h) => Type(k, x => f(h(x)))
        }
    }
}

trait KeyFunctions extends InjectFunctions {
  def del[F[_]: Functor](keys: NonEmptyList[ByteString])(implicit I: Inject[KeyAlgebra, F]): Free[F, Long] =
    inject[F, KeyAlgebra, Long](Del(keys, Return(_)))

  def dump[F[_]: Functor](key: ByteString)(implicit I: Inject[KeyAlgebra, F]): Free[F, Option[ByteString]] =
    inject[F, KeyAlgebra, Option[ByteString]](Dump(key, Return(_)))

  def exists[F[_]: Functor](key: ByteString)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Exists(key, Return(_)))

  def expire[F[_]: Functor](key: ByteString, in: Seconds)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Expire(key, in, Return(_)))

  def expireat[F[_]: Functor](key: ByteString, at: Seconds)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Expireat(key, at, Return(_)))

  def keys[F[_]: Functor](pattern: ByteString)(implicit I: Inject[KeyAlgebra, F]): Free[F, Seq[ByteString]] =
    inject[F, KeyAlgebra, Seq[ByteString]](Keys(pattern, Return(_)))

  def migrate[F[_]: Functor](
    host: ByteString,
    port: Int,
    key: ByteString,
    timeout: Milliseconds,
    destination: Short,
    copy: Boolean = false,
    replace: Boolean = false)(implicit I: Inject[KeyAlgebra, F]): Free[F, Status] =
    inject[F, KeyAlgebra, Status](Migrate(host, port, key, timeout, destination, copy, replace, Return(_)))

  def move[F[_]: Functor](key: ByteString, db: Short)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Move(key, db, Return(_)))

  def `object`[F[_]: Functor](subcommand: ObjectSubcommand)(implicit I: Inject[KeyAlgebra, F]): Free[F, Option[ObjectResult]] =
    inject[F, KeyAlgebra, Option[ObjectResult]](Object(subcommand, Return(_)))

  def persist[F[_]: Functor](key: ByteString)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Persist(key, Return(_)))

  def pexpire[F[_]: Functor](key: ByteString, in: Milliseconds)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Pexpire(key, in, Return(_)))

  def pexpireat[F[_]: Functor](key: ByteString, at: Milliseconds)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Pexpireat(key, at, Return(_)))

  def pttl[F[_]: Functor](key: ByteString)(implicit I: Inject[KeyAlgebra, F]): Free[F, Option[Milliseconds]] =
    inject[F, KeyAlgebra, Option[Milliseconds]](Pttl(key, Return(_)))

  def randomkey[F[_]: Functor](implicit I: Inject[KeyAlgebra, F]): Free[F, Option[ByteString]] =
    inject[F, KeyAlgebra, Option[ByteString]](Randomkey(Return(_)))

  def rename[F[_]: Functor](key: ByteString, name: ByteString)(implicit I: Inject[KeyAlgebra, F]): Free[F, Status] =
    inject[F, KeyAlgebra, Status](Rename(key, name, Return(_)))

  def renamenx[F[_]: Functor](key: ByteString, name: ByteString)(implicit I: Inject[KeyAlgebra, F]): Free[F, Boolean] =
    inject[F, KeyAlgebra, Boolean](Renamenx(key, name, Return(_)))

  def restore[F[_]: Functor](key: ByteString, value: ByteString, ttl: Option[Milliseconds] = None)(implicit I: Inject[KeyAlgebra, F]): Free[F, Status] =
    inject[F, KeyAlgebra, Status](Restore(key, ttl, value, Return(_)))

  def sort[F[_]: Functor](
    key: ByteString,
    by: Option[By] = None,
    limit: Option[Limit] = None,
    get: Seq[ByteString] = Nil,
    order: Order = Asc,
    alpha: Boolean = false,
    store: Option[ByteString] = None)(implicit I: Inject[KeyAlgebra, F]): Free[F, Seq[ByteString] \/ Long] =
    inject[F, KeyAlgebra, Seq[ByteString] \/ Long](Sort(key, by, limit, get, order, alpha, store, Return(_)))

  def ttl[F[_]: Functor](key: ByteString)(implicit I: Inject[KeyAlgebra, F]): Free[F, Option[Seconds]] =
    inject[F, KeyAlgebra, Option[Seconds]](Ttl(key, Return(_)))

  def `type`[F[_]: Functor](key: ByteString)(implicit I: Inject[KeyAlgebra, F]): Free[F, Option[DataType]] =
    inject[F, KeyAlgebra, Option[DataType]](Type(key, Return(_)))
}
