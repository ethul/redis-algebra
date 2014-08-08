package redis
package algebra

import scalaz.{Free, Functor, Inject, InjectFunctions, NonEmptyList}

import data.{Aggregate, Endpoint, Limit, Sum}

sealed abstract class ZSetAlgebra[A]

final case class Zadd[A](key: ByteString, pairs: NonEmptyList[(Double, ByteString)], h: Long => A) extends ZSetAlgebra[A]

final case class Zcard[A](key: ByteString, h: Long => A) extends ZSetAlgebra[A]

final case class Zcount[A](key: ByteString, min: Endpoint, max: Endpoint, h: Long => A) extends ZSetAlgebra[A]

final case class Zincrby[A](key: ByteString, increment: Double, member: ByteString, h: Double => A) extends ZSetAlgebra[A]

final case class Zinterstore[A](destination: ByteString, keys: NonEmptyList[ByteString], weights: Option[NonEmptyList[Double]], aggregate: Aggregate, h: Long => A) extends ZSetAlgebra[A]

final case class Zrange[A](key: ByteString, start: Long, stop: Long, withScores: Boolean, h: Seq[(ByteString, Option[Double])] => A) extends ZSetAlgebra[A]

final case class Zrangebyscore[A](key: ByteString, min: Endpoint, max: Endpoint, withScores: Boolean, limit: Option[Limit], h: Seq[(ByteString, Option[Double])] => A) extends ZSetAlgebra[A]

final case class Zrank[A](key: ByteString, member: ByteString, h: Option[Long] => A) extends ZSetAlgebra[A]

final case class Zrem[A](key: ByteString, members: NonEmptyList[ByteString], h: Long => A) extends ZSetAlgebra[A]

final case class Zremrangebyrank[A](key: ByteString, start: Long, stop: Long, h: Long => A) extends ZSetAlgebra[A]

final case class Zremrangebyscore[A](key: ByteString, start: Endpoint, stop: Endpoint, h: Long => A) extends ZSetAlgebra[A]

final case class Zrevrange[A](key: ByteString, start: Long, stop: Long, withScores: Boolean, h: Seq[(ByteString, Option[Double])] => A) extends ZSetAlgebra[A]

final case class Zrevrangebyscore[A](key: ByteString, min: Endpoint, max: Endpoint, withScores: Boolean, limit: Option[Limit], h: Seq[(ByteString, Option[Double])] => A) extends ZSetAlgebra[A]

final case class Zrevrank[A](key: ByteString, member: ByteString, h: Option[Long] => A) extends ZSetAlgebra[A]

final case class Zscore[A](key: ByteString, member: ByteString, h: Option[Double] => A) extends ZSetAlgebra[A]

final case class Zunionstore[A](destination: ByteString, keys: NonEmptyList[ByteString], weights: Option[NonEmptyList[Double]], aggregate: Aggregate, h: Long => A) extends ZSetAlgebra[A]

trait ZSetInstances {
  implicit val zsetAlgebraFunctor: Functor[ZSetAlgebra] =
    new Functor[ZSetAlgebra] {
      def map[A, B](a: ZSetAlgebra[A])(f: A => B): ZSetAlgebra[B] =
        a match {
          case Zadd(k, p, h) => Zadd(k, p, x => f(h(x)))
          case Zcard(k, h) => Zcard(k, x => f(h(x)))
          case Zcount(k, m, n, h) => Zcount(k, m, n, x => f(h(x)))
          case Zincrby(k, i, m, h) => Zincrby(k, i, m, x => f(h(x)))
          case Zinterstore(d, k, w, g, h) => Zinterstore(d, k, w, g, x => f(h(x)))
          case Zrange(k, s, t, w, h) => Zrange(k, s, t, w, x => f(h(x)))
          case Zrangebyscore(k, m, n, w, l, h) => Zrangebyscore(k, m, n, w, l, x => f(h(x)))
          case Zrank(k, m, h) => Zrank(k, m, x => f(h(x)))
          case Zrem(k, m, h) => Zrem(k, m, x => f(h(x)))
          case Zremrangebyrank(k, s, t, h) => Zremrangebyrank(k, s, t, x => f(h(x)))
          case Zremrangebyscore(k, s, t, h) => Zremrangebyscore(k, s, t, x => f(h(x)))
          case Zrevrange(k, s, t, w, h) => Zrevrange(k, s, t, w, x => f(h(x)))
          case Zrevrangebyscore(k, m, n, w, l, h) => Zrevrangebyscore(k, m, n, w, l, x => f(h(x)))
          case Zrevrank(k, m, h) => Zrevrank(k, m, x => f(h(x)))
          case Zscore(k, m, h) => Zscore(k, m, x => f(h(x)))
          case Zunionstore(d, k, w, g, h) => Zunionstore(d, k, w, g, x => f(h(x)))
        }
    }
}

trait ZSetFunctions extends InjectFunctions {
  def zadd[F[_]: Functor](key: ByteString, pairs: NonEmptyList[(Double, ByteString)])(implicit I: Inject[ZSetAlgebra, F]): Free[F, Long] =
    inject[F, ZSetAlgebra, Long](Zadd(key, pairs, Free.point(_)))

  def zcard[F[_]: Functor](key: ByteString)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Long] =
    inject[F, ZSetAlgebra, Long](Zcard(key, Free.point(_)))

  def zcount[F[_]: Functor](key: ByteString, min: Endpoint, max: Endpoint)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Long] =
    inject[F, ZSetAlgebra, Long](Zcount(key, min, max, Free.point(_)))

  def zincrby[F[_]: Functor](key: ByteString, increment: Double, member: ByteString)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Double] =
    inject[F, ZSetAlgebra, Double](Zincrby(key, increment, member, Free.point(_)))

  def zinterstore[F[_]: Functor](
    destination: ByteString,
    keys: NonEmptyList[ByteString],
    weights: Option[NonEmptyList[Double]] = None,
    aggregate: Aggregate = Sum)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Double] =
    inject[F, ZSetAlgebra, Double](Zinterstore(destination, keys, weights, aggregate, Free.point(_)))

  def zrange[F[_]: Functor](
    key: ByteString,
    start: Long,
    stop: Long,
    withScores: Boolean = false)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Seq[(ByteString, Option[Double])]] =
    inject[F, ZSetAlgebra, Seq[(ByteString, Option[Double])]](Zrange(key, start, stop, withScores, Free.point(_)))

  def zrangebyscore[F[_]: Functor](
    key: ByteString,
    min: Endpoint,
    max: Endpoint,
    withScores: Boolean = false,
    limit: Option[Limit] = None)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Seq[(ByteString, Option[Double])]] =
    inject[F, ZSetAlgebra, Seq[(ByteString, Option[Double])]](Zrangebyscore(key, min, max, withScores, limit, Free.point(_)))

  def zrank[F[_]: Functor](key: ByteString, member: ByteString)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Option[Long]] =
    inject[F, ZSetAlgebra, Option[Long]](Zrank(key, member, Free.point(_)))

  def zrem[F[_]: Functor](key: ByteString, members: NonEmptyList[ByteString])(implicit I: Inject[ZSetAlgebra, F]): Free[F, Long] =
    inject[F, ZSetAlgebra, Long](Zrem(key, members, Free.point(_)))

  def zremrangebyrank[F[_]: Functor](key: ByteString, start: Long, stop: Long)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Long] =
    inject[F, ZSetAlgebra, Long](Zremrangebyrank(key, start, stop, Free.point(_)))

  def zremrangebyscore[F[_]: Functor](key: ByteString, start: Endpoint, stop: Endpoint)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Long] =
    inject[F, ZSetAlgebra, Long](Zremrangebyscore(key, start, stop, Free.point(_)))

  def zrevrange[F[_]: Functor](
    key: ByteString,
    start: Long,
    stop: Long,
    withScores: Boolean = false)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Seq[(ByteString, Option[Double])]] =
    inject[F, ZSetAlgebra, Seq[(ByteString, Option[Double])]](Zrevrange(key, start, stop, withScores, Free.point(_)))

  def zrevrangebyscore[F[_]: Functor](
    key: ByteString,
    min: Endpoint,
    max: Endpoint,
    withScores: Boolean = false,
    limit: Option[Limit] = None)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Seq[(ByteString, Option[Double])]] =
    inject[F, ZSetAlgebra, Seq[(ByteString, Option[Double])]](Zrevrangebyscore(key, min, max, withScores, limit, Free.point(_)))

  def zrevrank[F[_]: Functor](key: ByteString, member: ByteString)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Option[Long]] =
    inject[F, ZSetAlgebra, Option[Long]](Zrevrank(key, member, Free.point(_)))

  def zscore[F[_]: Functor](key: ByteString, member: ByteString)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Option[Double]] =
    inject[F, ZSetAlgebra, Option[Double]](Zscore(key, member, Free.point(_)))

  def zunionstore[F[_]: Functor](
    destination: ByteString,
    keys: NonEmptyList[ByteString],
    weights: Option[NonEmptyList[Double]] = None,
    aggregate: Aggregate = Sum)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Double] =
    inject[F, ZSetAlgebra, Double](Zunionstore(destination, keys, weights, aggregate, Free.point(_)))
}
