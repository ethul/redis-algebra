package redis
package algebra

import scala.collection.immutable.{Set => ScalaSet}

import scalaz.{Free, Functor, NonEmptyList}, Free.Return

import typeclass.Inject, Inject._

import SetAlgebra._

sealed trait SetAlgebra[A]

final case class Sadd[A](key: String, members: NonEmptyList[String], h: Long => A) extends SetAlgebra[A]

final case class Scard[A](key: String, h: Long => A) extends SetAlgebra[A]

final case class Sdiff[A](keys: NonEmptyList[String], h: ScalaSet[String] => A) extends SetAlgebra[A]

final case class Sdiffstore[A](destination: String, keys: NonEmptyList[String], h: Long => A) extends SetAlgebra[A]

final case class Sinter[A](keys: NonEmptyList[String], h: ScalaSet[String] => A) extends SetAlgebra[A]

final case class Sinterstore[A](destination: String, keys: NonEmptyList[String], h: Long => A) extends SetAlgebra[A]

final case class Sismember[A](key: String, member: String, h: Boolean => A) extends SetAlgebra[A]

final case class Smembers[A](key: String, h: ScalaSet[String] => A) extends SetAlgebra[A]

final case class Smove[A](source: String, destination: String, member: String, h: Boolean => A) extends SetAlgebra[A]

final case class Spop[A](key: String, h: Option[String] => A) extends SetAlgebra[A]

final case class Srandmember[A](key: String, count: Option[Long], h: ScalaSet[String] => A) extends SetAlgebra[A]

final case class Srem[A](key: String, members: NonEmptyList[String], h: Long => A) extends SetAlgebra[A]

final case class Sunion[A](keys: NonEmptyList[String], h: ScalaSet[String] => A) extends SetAlgebra[A]

final case class Sunionstore[A](destination: String, keys: NonEmptyList[String], h: Long => A) extends SetAlgebra[A]

sealed trait SetInstances {
  implicit val setAlgebraFunctor: Functor[SetAlgebra] =
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
  def sadd[F[_]: Functor](key: String, members: NonEmptyList[String])(implicit I: Inject[SetAlgebra, F]): Free[F, Long] =
    inject[F, SetAlgebra, Long](Sadd(key, members, Return(_)))

  def scard[F[_]: Functor](key: String)(implicit I: Inject[SetAlgebra, F]): Free[F, Long] =
    inject[F, SetAlgebra, Long](Scard(key, Return(_)))

  def sdiff[F[_]: Functor](keys: NonEmptyList[String])(implicit I: Inject[SetAlgebra, F]): Free[F, ScalaSet[String]] =
    inject[F, SetAlgebra, ScalaSet[String]](Sdiff(keys, Return(_)))

  def sdiffstore[F[_]: Functor](destination: String, keys: NonEmptyList[String])(implicit I: Inject[SetAlgebra, F]): Free[F, Long] =
    inject[F, SetAlgebra, Long](Sdiffstore(destination, keys, Return(_)))

  def sinter[F[_]: Functor](keys: NonEmptyList[String])(implicit I: Inject[SetAlgebra, F]): Free[F, ScalaSet[String]] =
    inject[F, SetAlgebra, ScalaSet[String]](Sinter(keys, Return(_)))

  def sinterstore[F[_]: Functor](destination: String, keys: NonEmptyList[String])(implicit I: Inject[SetAlgebra, F]): Free[F, Long] =
    inject[F, SetAlgebra, Long](Sinterstore(destination, keys, Return(_)))

  def sismember[F[_]: Functor](key: String, member: String)(implicit I: Inject[SetAlgebra, F]): Free[F, Boolean] =
    inject[F, SetAlgebra, Boolean](Sismember(key, member, Return(_)))

  def smembers[F[_]: Functor](key: String)(implicit I: Inject[SetAlgebra, F]): Free[F, ScalaSet[String]] =
    inject[F, SetAlgebra, ScalaSet[String]](Smembers(key, Return(_)))

  def smove[F[_]: Functor](source: String, destination: String, member: String)(implicit I: Inject[SetAlgebra, F]): Free[F, Boolean] =
    inject[F, SetAlgebra, Boolean](Smove(source, destination, member, Return(_)))

  def spop[F[_]: Functor](key: String)(implicit I: Inject[SetAlgebra, F]): Free[F, Option[String]] =
    inject[F, SetAlgebra, Option[String]](Spop(key, Return(_)))

  def srandmember[F[_]: Functor](key: String, count: Option[Long] = None)(implicit I: Inject[SetAlgebra, F]): Free[F, ScalaSet[String]] =
    inject[F, SetAlgebra, ScalaSet[String]](Srandmember(key, count, Return(_)))

  def srem[F[_]: Functor](key: String, members: NonEmptyList[String])(implicit I: Inject[SetAlgebra, F]): Free[F, Long] =
    inject[F, SetAlgebra, Long](Srem(key, members, Return(_)))

  def sunion[F[_]: Functor](keys: NonEmptyList[String])(implicit I: Inject[SetAlgebra, F]): Free[F, ScalaSet[String]] =
    inject[F, SetAlgebra, ScalaSet[String]](Sunion(keys, Return(_)))

  def sunionstore[F[_]: Functor](destination: String, keys: NonEmptyList[String])(implicit I: Inject[SetAlgebra, F]): Free[F, Long] =
    inject[F, SetAlgebra, Long](Sunionstore(destination, keys, Return(_)))
}

object SetAlgebra extends SetInstances with SetFunctions
