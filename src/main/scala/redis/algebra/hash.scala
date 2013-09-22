package redis
package algebra

import scalaz.{Free, Functor, Inject, InjectFunctions, NonEmptyList}, Free.Return

import data.Status

sealed abstract class HashAlgebra[A]

final case class Hdel[A](key: ByteString, fields: NonEmptyList[ByteString], h: Long => A) extends HashAlgebra[A]

final case class Hexists[A](key: ByteString, field: ByteString, h: Boolean => A) extends HashAlgebra[A]

final case class Hget[A](key: ByteString, field: ByteString, h: Option[ByteString] => A) extends HashAlgebra[A]

final case class Hgetall[A](key: ByteString, h: Seq[(ByteString, ByteString)] => A) extends HashAlgebra[A]

final case class Hincrby[A](key: ByteString, field: ByteString, increment: Long, h: Long => A) extends HashAlgebra[A]

final case class Hincrbyfloat[A](key: ByteString, field: ByteString, increment: BigDecimal, h: BigDecimal => A) extends HashAlgebra[A]

final case class Hkeys[A](key: ByteString, h: Seq[ByteString] => A) extends HashAlgebra[A]

final case class Hlen[A](key: ByteString, h: Long => A) extends HashAlgebra[A]

final case class Hmget[A](key: ByteString, fields: NonEmptyList[ByteString], h: Seq[Option[ByteString]] => A) extends HashAlgebra[A]

final case class Hmset[A](key: ByteString, pairs: NonEmptyList[(ByteString, ByteString)], h: Status => A) extends HashAlgebra[A]

final case class Hset[A](key: ByteString, field: ByteString, value: ByteString, h: Boolean => A) extends HashAlgebra[A]

final case class Hsetnx[A](key: ByteString, field: ByteString, value: ByteString, h: Boolean => A) extends HashAlgebra[A]

final case class Hvals[A](key: ByteString, h: Seq[ByteString] => A) extends HashAlgebra[A]

trait HashInstances {
  implicit val hashAlgebraFunctor: Functor[HashAlgebra] =
    new Functor[HashAlgebra] {
      def map[A, B](a: HashAlgebra[A])(f: A => B): HashAlgebra[B] =
        a match {
          case Hdel(k, s, h) => Hdel(k, s, x => f(h(x)))
          case Hexists(k, s, h) => Hexists(k, s, x => f(h(x)))
          case Hget(k, s, h) => Hget(k, s, x => f(h(x)))
          case Hgetall(k, h) => Hgetall(k, x => f(h(x)))
          case Hincrby(k, s, i, h) => Hincrby(k, s, i, x => f(h(x)))
          case Hincrbyfloat(k, s, i, h) => Hincrbyfloat(k, s, i, x => f(h(x)))
          case Hkeys(k, h) => Hkeys(k, x => f(h(x)))
          case Hlen(k, h) => Hlen(k, x => f(h(x)))
          case Hmget(k, s, h) => Hmget(k, s, x => f(h(x)))
          case Hmset(k, p, h) => Hmset(k, p, x => f(h(x)))
          case Hset(k, s, v, h) => Hset(k, s, v, x => f(h(x)))
          case Hsetnx(k, s, v, h) => Hsetnx(k, s, v, x => f(h(x)))
          case Hvals(k, h) => Hvals(k, x => f(h(x)))
        }
    }
}

trait HashFunctions extends InjectFunctions {
  def hdel[F[_]: Functor](key: ByteString, fields: NonEmptyList[ByteString])(implicit I: Inject[HashAlgebra, F]): Free[F, Long] =
    inject[F, HashAlgebra, Long](Hdel(key, fields, Return(_)))

  def hexists[F[_]: Functor](key: ByteString, field: ByteString)(implicit I: Inject[HashAlgebra, F]): Free[F, Boolean] =
    inject[F, HashAlgebra, Boolean](Hexists(key, field, Return(_)))

  def hget[F[_]: Functor](key: ByteString, field: ByteString)(implicit I: Inject[HashAlgebra, F]): Free[F, Option[ByteString]] =
    inject[F, HashAlgebra, Option[ByteString]](Hget(key, field, Return(_)))

  def hgetall[F[_]: Functor](key: ByteString)(implicit I: Inject[HashAlgebra, F]): Free[F, Seq[(ByteString, ByteString)]] =
    inject[F, HashAlgebra, Seq[(ByteString, ByteString)]](Hgetall(key, Return(_)))

  def hincrby[F[_]: Functor](key: ByteString, field: ByteString, increment: Long)(implicit I: Inject[HashAlgebra, F]): Free[F, Long] =
    inject[F, HashAlgebra, Long](Hincrby(key, field, increment, Return(_)))

  def hincrbyfloat[F[_]: Functor](key: ByteString, field: ByteString, increment: BigDecimal)(implicit I: Inject[HashAlgebra, F]): Free[F, BigDecimal] =
    inject[F, HashAlgebra, BigDecimal](Hincrbyfloat(key, field, increment, Return(_)))

  def hkeys[F[_]: Functor](key: ByteString)(implicit I: Inject[HashAlgebra, F]): Free[F, Seq[ByteString]] =
    inject[F, HashAlgebra, Seq[ByteString]](Hkeys(key, Return(_)))

  def hlen[F[_]: Functor](key: ByteString)(implicit I: Inject[HashAlgebra, F]): Free[F, Long] =
    inject[F, HashAlgebra, Long](Hlen(key, Return(_)))

  def hmget[F[_]: Functor](key: ByteString, fields: NonEmptyList[ByteString])(implicit I: Inject[HashAlgebra, F]): Free[F, Seq[Option[ByteString]]] =
    inject[F, HashAlgebra, Seq[Option[ByteString]]](Hmget(key, fields, Return(_)))

  def hmset[F[_]: Functor](key: ByteString, pairs: NonEmptyList[(ByteString, ByteString)])(implicit I: Inject[HashAlgebra, F]): Free[F, Status] =
    inject[F, HashAlgebra, Status](Hmset(key, pairs, Return(_)))

  def hset[F[_]: Functor](key: ByteString, field: ByteString, value: ByteString)(implicit I: Inject[HashAlgebra, F]): Free[F, Boolean] =
    inject[F, HashAlgebra, Boolean](Hset(key, field, value, Return(_)))

  def hsetnx[F[_]: Functor](key: ByteString, field: ByteString, value: ByteString)(implicit I: Inject[HashAlgebra, F]): Free[F, Boolean] =
    inject[F, HashAlgebra, Boolean](Hsetnx(key, field, value, Return(_)))

  def hvals[F[_]: Functor](key: ByteString)(implicit I: Inject[HashAlgebra, F]): Free[F, Seq[ByteString]] =
    inject[F, HashAlgebra, Seq[ByteString]](Hvals(key, Return(_)))
}
