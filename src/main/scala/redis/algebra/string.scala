package redis
package algebra

import scalaz.{\/, Free, Functor, NonEmptyList}, Free.Return

import typeclass.Inject, Inject._

import StringAlgebra._

sealed trait StringAlgebra[A]

final case class Append[A](key: String, value: String, h: Long => A) extends StringAlgebra[A]

final case class Bitcount[A](key: String, start: Option[Int], end: Option[Int], h: Long => A) extends StringAlgebra[A]

final case class Bitop[A](operation: BitOperation, h: Long => A) extends StringAlgebra[A]

final case class Decr[A](key: String, h: Long => A) extends StringAlgebra[A]

final case class Decrby[A](key: String, decrement: Long, h: Long => A) extends StringAlgebra[A]

final case class Get[A](key: String, h: Option[String] => A) extends StringAlgebra[A]

final case class Getbit[A](key: String, offset: Int, h: Long => A) extends StringAlgebra[A]

final case class Getrange[A](key: String, start: Int, end: Int, h: String => A) extends StringAlgebra[A]

final case class Getset[A](key: String, value: String, h: Option[String] => A) extends StringAlgebra[A]

final case class Incr[A](key: String, h: Long => A) extends StringAlgebra[A]

final case class Incrby[A](key: String, increment: Long, h: Long => A) extends StringAlgebra[A]

final case class Incrbyfloat[A](key: String, increment: Float, h: Float => A) extends StringAlgebra[A]

final case class Mget[A](keys: NonEmptyList[String], h: Seq[Option[String]] => A) extends StringAlgebra[A]

final case class Mset[A](pairs: NonEmptyList[(String, String)], a: A) extends StringAlgebra[A]

final case class Msetnx[A](pairs: NonEmptyList[(String, String)], h: Boolean => A) extends StringAlgebra[A]

final case class Psetex[A](key: String, in: Milliseconds, value: String, a: A) extends StringAlgebra[A]

final case class Set[A](key: String, value: String, in: Option[Seconds \/ Milliseconds], option: Option[SetOption], h: Boolean => A) extends StringAlgebra[A]

final case class Setbit[A](key: String, offset: Int, value: String, h: Long => A) extends StringAlgebra[A]

final case class Setex[A](key: String, in: Seconds, value: String, a: A) extends StringAlgebra[A]

final case class Setnx[A](key: String, value: String, h: Boolean => A) extends StringAlgebra[A]

final case class Setrange[A](key: String, offset: Int, value: String, h: Long => A) extends StringAlgebra[A]

final case class Strlen[A](key: String, h: Long => A) extends StringAlgebra[A]

sealed trait BitOperation
final case class And(dest: String, keys: NonEmptyList[String]) extends BitOperation
final case class Or(dest: String, keys: NonEmptyList[String]) extends BitOperation
final case class Xor(dest: String, keys: NonEmptyList[String]) extends BitOperation
final case class Not(dest: String, key: String) extends BitOperation

sealed trait SetOption
case object Nx extends SetOption
case object Xx extends SetOption

sealed trait StringInstances {
  implicit val stringAlgebraFunctor: Functor[StringAlgebra] =
    new Functor[StringAlgebra] {
      def map[A, B](a: StringAlgebra[A])(f: A => B): StringAlgebra[B] = a match {
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
        case Mset(p, a) => Mset(p, f(a))
        case Msetnx(p, h) => Msetnx(p, x => f(h(x)))
        case Psetex(k, i, v, a) => Psetex(k, i, v, f(a))
        case Set(k, v, i, o, h) => Set(k, v, i, o, x => f(h(x)))
        case Setbit(k, o, v, h) => Setbit(k, o, v, x => f(h(x)))
        case Setex(k, i, v, a) => Setex(k, i, v, f(a))
        case Setnx(k, v, h) => Setnx(k, v, x => f(h(x)))
        case Setrange(k, o, v, h) => Setrange(k, o, v, x => f(h(x)))
        case Strlen(k, h) => Strlen(k, x => f(h(x)))
      }
    }
}

sealed trait StringFunctions {
  def append[F[_]: Functor](key: String, value: String)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Append(key, value, Return(_)))

  def bitcount[F[_]: Functor](key: String, start: Option[Int] = None, end: Option[Int] = None)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Bitcount(key, start, end, Return(_)))

  def bitop[F[_]: Functor](operation: BitOperation)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Bitop(operation, Return(_)))

  def decr[F[_]: Functor](key: String)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Decr(key, Return(_)))

  def decrby[F[_]: Functor](key: String, decrement: Long)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Decrby(key, decrement, Return(_)))

  def get[F[_]: Functor](key: String)(implicit I: Inject[StringAlgebra, F]): Free[F, Option[String]] =
    inject[F, StringAlgebra, Option[String]](Get(key, Return(_)))

  def getbit[F[_]: Functor](key: String, offset: Int)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Getbit(key, offset, Return(_)))

  def getrange[F[_]: Functor](key: String, start: Int, end: Int)(implicit I: Inject[StringAlgebra, F]): Free[F, String] =
    inject[F, StringAlgebra, String](Getrange(key, start, end, Return(_)))

  def getset[F[_]: Functor](key: String, value: String)(implicit I: Inject[StringAlgebra, F]): Free[F, Option[String]] =
    inject[F, StringAlgebra, Option[String]](Getset(key, value, Return(_)))

  def incr[F[_]: Functor](key: String)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Incr(key, Return(_)))

  def incrby[F[_]: Functor](key: String, increment: Long)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Incrby(key, increment, Return(_)))

  def incrbyfloat[F[_]: Functor](key: String, increment: Float)(implicit I: Inject[StringAlgebra, F]): Free[F, Float] =
    inject[F, StringAlgebra, Float](Incrbyfloat(key, increment, Return(_)))

  def mget[F[_]: Functor](keys: NonEmptyList[String])(implicit I: Inject[StringAlgebra, F]): Free[F, Seq[Option[String]]] =
    inject[F, StringAlgebra, Seq[Option[String]]](Mget(keys, Return(_)))

  def mset[F[_]: Functor](pairs: NonEmptyList[(String, String)])(implicit I: Inject[StringAlgebra, F]): Free[F, Unit] =
    inject[F, StringAlgebra, Unit](Mset(pairs, Return(())))

  def msetnx[F[_]: Functor](pairs: NonEmptyList[(String, String)])(implicit I: Inject[StringAlgebra, F]): Free[F, Boolean] =
    inject[F, StringAlgebra, Boolean](Msetnx(pairs, Return(_)))

  def psetex[F[_]: Functor](key: String, in: Milliseconds, value: String)(implicit I: Inject[StringAlgebra, F]): Free[F, Unit] =
    inject[F, StringAlgebra, Unit](Psetex(key, in, value, Return(())))

  def set[F[_]: Functor](key: String, value: String,
    in: Option[Seconds \/ Milliseconds] = None, option: Option[SetOption] = None)(implicit I: Inject[StringAlgebra, F]): Free[F, Boolean] =
    inject[F, StringAlgebra, Boolean](Set(key, value, in, option, Return(_)))

  def setbit[F[_]: Functor](key: String, offset: Int, value: String)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Setbit(key, offset, value, Return(_)))

  def setex[F[_]: Functor](key: String, in: Seconds, value: String)(implicit I: Inject[StringAlgebra, F]): Free[F, Unit] =
    inject[F, StringAlgebra, Unit](Setex(key, in, value, Return(())))

  def setnx[F[_]: Functor](key: String, value: String)(implicit I: Inject[StringAlgebra, F]): Free[F, Boolean] =
    inject[F, StringAlgebra, Boolean](Setnx(key, value, Return(_)))

  def setrange[F[_]: Functor](key: String, offset: Int, value: String)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Setrange(key, offset, value, Return(_)))

  def strlen[F[_]: Functor](key: String)(implicit I: Inject[StringAlgebra, F]): Free[F, Long] =
    inject[F, StringAlgebra, Long](Strlen(key, Return(_)))
}

object StringAlgebra extends StringInstances with StringFunctions
