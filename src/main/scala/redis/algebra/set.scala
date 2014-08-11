package redis
package algebra

import scala.collection.immutable.{Set => ScalaSet}

import scalaz.{Free, Functor, Inject, InjectFunctions, NonEmptyList}

sealed abstract class SetAlgebra[A]

final case class Sadd[A](key: ByteString, members: NonEmptyList[ByteString], h: Long => A) extends SetAlgebra[A]

final case class Scard[A](key: ByteString, h: Long => A) extends SetAlgebra[A]

final case class Sdiff[A](keys: NonEmptyList[ByteString], h: ScalaSet[ByteString] => A) extends SetAlgebra[A]

final case class Sdiffstore[A](destination: ByteString, keys: NonEmptyList[ByteString], h: Long => A) extends SetAlgebra[A]

final case class Sinter[A](keys: NonEmptyList[ByteString], h: ScalaSet[ByteString] => A) extends SetAlgebra[A]

final case class Sinterstore[A](destination: ByteString, keys: NonEmptyList[ByteString], h: Long => A) extends SetAlgebra[A]

final case class Sismember[A](key: ByteString, member: ByteString, h: Boolean => A) extends SetAlgebra[A]

final case class Smembers[A](key: ByteString, h: ScalaSet[ByteString] => A) extends SetAlgebra[A]

final case class Smove[A](source: ByteString, destination: ByteString, member: ByteString, h: Boolean => A) extends SetAlgebra[A]

final case class Spop[A](key: ByteString, h: Option[ByteString] => A) extends SetAlgebra[A]

final case class Srandmember[A](key: ByteString, count: Option[Long], h: ScalaSet[ByteString] => A) extends SetAlgebra[A]

final case class Srem[A](key: ByteString, members: NonEmptyList[ByteString], h: Long => A) extends SetAlgebra[A]

final case class Sunion[A](keys: NonEmptyList[ByteString], h: ScalaSet[ByteString] => A) extends SetAlgebra[A]

final case class Sunionstore[A](destination: ByteString, keys: NonEmptyList[ByteString], h: Long => A) extends SetAlgebra[A]

trait SetInstances {
  implicit val setAlgebraFunctor: Functor[SetAlgebra] =
    new Functor[SetAlgebra] {
      def map[A, B](a: SetAlgebra[A])(f: A => B): SetAlgebra[B] =
        a match {
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

trait SetFunctions extends InjectFunctions {
  def sadd[F[_]: Functor](key: ByteString, members: NonEmptyList[ByteString])(implicit I: Inject[SetAlgebra, F]): Free[F, Long] =
    inject[F, SetAlgebra, Long](Sadd(key, members, Free.point(_)))

  def scard[F[_]: Functor](key: ByteString)(implicit I: Inject[SetAlgebra, F]): Free[F, Long] =
    inject[F, SetAlgebra, Long](Scard(key, Free.point(_)))

  def sdiff[F[_]: Functor](keys: NonEmptyList[ByteString])(implicit I: Inject[SetAlgebra, F]): Free[F, ScalaSet[ByteString]] =
    inject[F, SetAlgebra, ScalaSet[ByteString]](Sdiff(keys, Free.point(_)))

  def sdiffstore[F[_]: Functor](destination: ByteString, keys: NonEmptyList[ByteString])(implicit I: Inject[SetAlgebra, F]): Free[F, Long] =
    inject[F, SetAlgebra, Long](Sdiffstore(destination, keys, Free.point(_)))

  def sinter[F[_]: Functor](keys: NonEmptyList[ByteString])(implicit I: Inject[SetAlgebra, F]): Free[F, ScalaSet[ByteString]] =
    inject[F, SetAlgebra, ScalaSet[ByteString]](Sinter(keys, Free.point(_)))

  def sinterstore[F[_]: Functor](destination: ByteString, keys: NonEmptyList[ByteString])(implicit I: Inject[SetAlgebra, F]): Free[F, Long] =
    inject[F, SetAlgebra, Long](Sinterstore(destination, keys, Free.point(_)))

  def sismember[F[_]: Functor](key: ByteString, member: ByteString)(implicit I: Inject[SetAlgebra, F]): Free[F, Boolean] =
    inject[F, SetAlgebra, Boolean](Sismember(key, member, Free.point(_)))

  def smembers[F[_]: Functor](key: ByteString)(implicit I: Inject[SetAlgebra, F]): Free[F, ScalaSet[ByteString]] =
    inject[F, SetAlgebra, ScalaSet[ByteString]](Smembers(key, Free.point(_)))

  def smove[F[_]: Functor](source: ByteString, destination: ByteString, member: ByteString)(implicit I: Inject[SetAlgebra, F]): Free[F, Boolean] =
    inject[F, SetAlgebra, Boolean](Smove(source, destination, member, Free.point(_)))

  def spop[F[_]: Functor](key: ByteString)(implicit I: Inject[SetAlgebra, F]): Free[F, Option[ByteString]] =
    inject[F, SetAlgebra, Option[ByteString]](Spop(key, Free.point(_)))

  def srandmember[F[_]: Functor](key: ByteString, count: Option[Long] = None)(implicit I: Inject[SetAlgebra, F]): Free[F, ScalaSet[ByteString]] =
    inject[F, SetAlgebra, ScalaSet[ByteString]](Srandmember(key, count, Free.point(_)))

  def srem[F[_]: Functor](key: ByteString, members: NonEmptyList[ByteString])(implicit I: Inject[SetAlgebra, F]): Free[F, Long] =
    inject[F, SetAlgebra, Long](Srem(key, members, Free.point(_)))

  def sunion[F[_]: Functor](keys: NonEmptyList[ByteString])(implicit I: Inject[SetAlgebra, F]): Free[F, ScalaSet[ByteString]] =
    inject[F, SetAlgebra, ScalaSet[ByteString]](Sunion(keys, Free.point(_)))

  def sunionstore[F[_]: Functor](destination: ByteString, keys: NonEmptyList[ByteString])(implicit I: Inject[SetAlgebra, F]): Free[F, Long] =
    inject[F, SetAlgebra, Long](Sunionstore(destination, keys, Free.point(_)))
}
