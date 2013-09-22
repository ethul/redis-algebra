package redis
package algebra

import scalaz.{\/, Free, Functor, Inject, InjectFunctions, NonEmptyList}, Free.Return

import data.{BitOperation, SetOption, Status}

sealed abstract class StringAlgebra[A]

final case class Append[A](key: ByteString, value: ByteString, h: Long => A) extends StringAlgebra[A]

final case class Bitcount[A](key: ByteString, start: Option[Long], end: Option[Long], h: Long => A) extends StringAlgebra[A]

final case class Bitop[A](operation: BitOperation, h: Long => A) extends StringAlgebra[A]

final case class Decr[A](key: ByteString, h: Long => A) extends StringAlgebra[A]

final case class Decrby[A](key: ByteString, decrement: Long, h: Long => A) extends StringAlgebra[A]

final case class Get[A](key: ByteString, h: Option[ByteString] => A) extends StringAlgebra[A]

final case class Getbit[A](key: ByteString, offset: Long, h: Boolean => A) extends StringAlgebra[A]

final case class Getrange[A](key: ByteString, start: Long, end: Long, h: ByteString => A) extends StringAlgebra[A]

final case class Getset[A](key: ByteString, value: ByteString, h: Option[ByteString] => A) extends StringAlgebra[A]

final case class Incr[A](key: ByteString, h: Long => A) extends StringAlgebra[A]

final case class Incrby[A](key: ByteString, increment: Long, h: Long => A) extends StringAlgebra[A]

final case class Incrbyfloat[A](key: ByteString, increment: BigDecimal, h: BigDecimal => A) extends StringAlgebra[A]

final case class Mget[A](keys: NonEmptyList[ByteString], h: Seq[Option[ByteString]] => A) extends StringAlgebra[A]

final case class Mset[A](pairs: NonEmptyList[(ByteString, ByteString)], h: Status => A) extends StringAlgebra[A]

final case class Msetnx[A](pairs: NonEmptyList[(ByteString, ByteString)], h: Boolean => A) extends StringAlgebra[A]

final case class Psetex[A](key: ByteString, in: Milliseconds, value: ByteString, h: Status => A) extends StringAlgebra[A]

final case class Set[A](key: ByteString, value: ByteString, in: Option[Seconds \/ Milliseconds], option: Option[SetOption], h: Boolean => A) extends StringAlgebra[A]

final case class Setbit[A](key: ByteString, offset: Long, value: Boolean, h: Long => A) extends StringAlgebra[A]

final case class Setex[A](key: ByteString, in: Seconds, value: ByteString, h: Status => A) extends StringAlgebra[A]

final case class Setnx[A](key: ByteString, value: ByteString, h: Boolean => A) extends StringAlgebra[A]

final case class Setrange[A](key: ByteString, offset: Long, value: ByteString, h: Long => A) extends StringAlgebra[A]

final case class Strlen[A](key: ByteString, h: Long => A) extends StringAlgebra[A]

trait StringInstances {
  implicit val stringAlgebraFunctor: Functor[StringAlgebra] =
    new Functor[StringAlgebra] {
      def map[A, B](a: StringAlgebra[A])(f: A => B): StringAlgebra[B] =
        a match {
          case Append(k, v, h) => Append(k, v, x => f(h(x)))
          case Bitcount(k, s, e, h) => Bitcount(k, s, e, x => f(h(x)))
          case Bitop(o, h) => Bitop(o, x => f(h(x)))
          case Decr(k, h) => Decr(k, x => f(h(x)))
          case Decrby(k, d, h) => Decrby(k, d, x => f(h(x)))
          case Get(k, h) => Get(k, x => f(h(x)))
          case Getbit(k, o, h) => Getbit(k, o, x => f(h(x)))
          case Getrange(k, s, e, h) => Getrange(k, s, e, x => f(h(x)))
          case Getset(k, v, h) => Getset(k, v, x => f(h(x)))
          case Incr(k, h) => Incr(k, x => f(h(x)))
          case Incrby(k, i, h) => Incrby(k, i, x => f(h(x)))
          case Incrbyfloat(k, i, h) => Incrbyfloat(k, i, x => f(h(x)))
          case Mget(k, h) => Mget(k, x => f(h(x)))
          case Mset(p, h) => Mset(p, x => f(h(x)))
          case Msetnx(p, h) => Msetnx(p, x => f(h(x)))
          case Psetex(k, i, v, h) => Psetex(k, i, v, x => f(h(x)))
          case Set(k, v, i, o, h) => Set(k, v, i, o, x => f(h(x)))
          case Setbit(k, o, v, h) => Setbit(k, o, v, x => f(h(x)))
          case Setex(k, i, v, h) => Setex(k, i, v, x => f(h(x)))
          case Setnx(k, v, h) => Setnx(k, v, x => f(h(x)))
          case Setrange(k, o, v, h) => Setrange(k, o, v, x => f(h(x)))
          case Strlen(k, h) => Strlen(k, x => f(h(x)))
        }
    }
}

trait StringFunctions extends InjectFunctions {
  def append[F[_]: Functor](key: ByteString, value: ByteString)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Append(key, value, Return(_)))

  def bitcount[F[_]: Functor](
    key: ByteString,
    start: Option[Long] = None,
    end: Option[Long] = None)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Bitcount(key, start, end, Return(_)))

  def bitop[F[_]: Functor](operation: BitOperation)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Bitop(operation, Return(_)))

  def decr[F[_]: Functor](key: ByteString)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Decr(key, Return(_)))

  def decrby[F[_]: Functor](key: ByteString, decrement: Long)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Decrby(key, decrement, Return(_)))

  def get[F[_]: Functor](key: ByteString)(implicit I: Inject[StringAlgebra, F]): Free[F, Option[ByteString]] =
    inject[F, StringAlgebra, Option[ByteString]](Get(key, Return(_)))

  def getbit[F[_]: Functor](key: ByteString, offset: Long)(implicit I: Inject[StringAlgebra, F]): Free[F, Boolean] =
    inject[F, StringAlgebra, Boolean](Getbit(key, offset, Return(_)))

  def getrange[F[_]: Functor](key: ByteString, start: Long, end: Long)(implicit I: Inject[StringAlgebra, F]): Free[F, ByteString] =
    inject[F, StringAlgebra, ByteString](Getrange(key, start, end, Return(_)))

  def getset[F[_]: Functor](key: ByteString, value: ByteString)(implicit I: Inject[StringAlgebra, F]): Free[F, Option[ByteString]] =
    inject[F, StringAlgebra, Option[ByteString]](Getset(key, value, Return(_)))

  def incr[F[_]: Functor](key: ByteString)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Incr(key, Return(_)))

  def incrby[F[_]: Functor](key: ByteString, increment: Long)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Incrby(key, increment, Return(_)))

  def incrbyfloat[F[_]: Functor](key: ByteString, increment: BigDecimal)(implicit I: Inject[StringAlgebra, F]): Free[F, BigDecimal] =
    inject[F, StringAlgebra, BigDecimal](Incrbyfloat(key, increment, Return(_)))

  def mget[F[_]: Functor](keys: NonEmptyList[ByteString])(implicit I: Inject[StringAlgebra, F]): Free[F, Seq[Option[ByteString]]] =
    inject[F, StringAlgebra, Seq[Option[ByteString]]](Mget(keys, Return(_)))

  def mset[F[_]: Functor](pairs: NonEmptyList[(ByteString, ByteString)])(implicit I: Inject[StringAlgebra, F]): Free[F, Status] =
    inject[F, StringAlgebra, Status](Mset(pairs, Return(_)))

  def msetnx[F[_]: Functor](pairs: NonEmptyList[(ByteString, ByteString)])(implicit I: Inject[StringAlgebra, F]): Free[F, Boolean] =
    inject[F, StringAlgebra, Boolean](Msetnx(pairs, Return(_)))

  def psetex[F[_]: Functor](key: ByteString, in: Milliseconds, value: ByteString)(implicit I: Inject[StringAlgebra, F]): Free[F, Status] =
    inject[F, StringAlgebra, Status](Psetex(key, in, value, Return(_)))

  def set[F[_]: Functor](
    key: ByteString,
    value: ByteString,
    in: Option[Seconds \/ Milliseconds] = None,
    option: Option[SetOption] = None)(implicit I: Inject[StringAlgebra, F]): Free[F, Boolean] =
    inject[F, StringAlgebra, Boolean](Set(key, value, in, option, Return(_)))

  def setbit[F[_]: Functor](key: ByteString, offset: Long, value: Boolean)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Setbit(key, offset, value, Return(_)))

  def setex[F[_]: Functor](key: ByteString, in: Seconds, value: ByteString)(implicit I: Inject[StringAlgebra, F]): Free[F, Status] =
    inject[F, StringAlgebra, Status](Setex(key, in, value, Return(_)))

  def setnx[F[_]: Functor](key: ByteString, value: ByteString)(implicit I: Inject[StringAlgebra, F]): Free[F, Boolean] =
    inject[F, StringAlgebra, Boolean](Setnx(key, value, Return(_)))

  def setrange[F[_]: Functor](key: ByteString, offset: Long, value: ByteString)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Setrange(key, offset, value, Return(_)))

  def strlen[F[_]: Functor](key: ByteString)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Strlen(key, Return(_)))
}
