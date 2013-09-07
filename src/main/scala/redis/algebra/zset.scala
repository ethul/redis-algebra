package redis
package algebra

import scalaz.{Free, Functor, NonEmptyList}, Free.Return

import typeclass.Inject, Inject._

import ZSetAlgebra._

sealed trait ZSetAlgebra[A]

final case class Zadd[A](key: String, pairs: NonEmptyList[(Double, String)], h: Long => A) extends ZSetAlgebra[A]

final case class Zcard[A](key: String, h: Long => A) extends ZSetAlgebra[A]

final case class Zcount[A](key: String, min: Endpoint, max: Endpoint, h: Long => A) extends ZSetAlgebra[A]

final case class Zincrby[A](key: String, increment: Double, member: String, h: Double => A) extends ZSetAlgebra[A]

final case class Zinterstore[A](destination: String, keys: NonEmptyList[String], weights: Option[NonEmptyList[Double]], aggregate: Aggregate, h: Long => A) extends ZSetAlgebra[A]

final case class Zrange[A](key: String, start: Long, stop: Long, withScores: Boolean, h: Seq[(String, Option[Double])] => A) extends ZSetAlgebra[A]

final case class Zrangebyscore[A](key: String, min: Endpoint, max: Endpoint, withScores: Boolean, limit: Option[Limit], h: Seq[(String, Option[Double])] => A) extends ZSetAlgebra[A]

final case class Zrank[A](key: String, member: String, h: Option[Long] => A) extends ZSetAlgebra[A]

final case class Zrem[A](key: String, members: NonEmptyList[String], h: Long => A) extends ZSetAlgebra[A]

final case class Zremrangebyrank[A](key: String, start: Long, stop: Long, h: Long => A) extends ZSetAlgebra[A]

final case class Zremrangebyscore[A](key: String, start: Endpoint, stop: Endpoint, h: Long => A) extends ZSetAlgebra[A]

final case class Zrevrange[A](key: String, start: Long, stop: Long, withScores: Boolean, h: Seq[(String, Option[Double])] => A) extends ZSetAlgebra[A]

final case class Zrevrangebyscore[A](key: String, min: Endpoint, max: Endpoint, withScores: Boolean, limit: Option[Limit], h: Seq[(String, Option[Double])] => A) extends ZSetAlgebra[A]

final case class Zrevrank[A](key: String, member: String, h: Option[Long] => A) extends ZSetAlgebra[A]

final case class Zscore[A](key: String, member: String, h: Option[Double] => A) extends ZSetAlgebra[A]

final case class Zunionstore[A](destination: String, keys: NonEmptyList[String], weights: Option[NonEmptyList[Double]], aggregate: Aggregate, h: Long => A) extends ZSetAlgebra[A]

sealed trait ZSetTypes {
  sealed trait Endpoint
  case class Closed(value: Double) extends Endpoint
  case class Open(value: Double) extends Endpoint
  case object -∞ extends Endpoint
  case object +∞ extends Endpoint

  sealed trait Aggregate
  case object Sum extends Aggregate
  case object Min extends Aggregate
  case object Max extends Aggregate

  case class Limit(offset: Long, count: Long)
}

sealed trait ZSetInstances {
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

sealed trait ZSetFunctions {
  def zadd[F[_]: Functor](key: String, pairs: NonEmptyList[(Double, String)])(implicit I: Inject[ZSetAlgebra, F]): Free[F, Long] =
    inject[F, ZSetAlgebra, Long](Zadd(key, pairs, Return(_)))

  def zcard[F[_]: Functor](key: String)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Long] =
    inject[F, ZSetAlgebra, Long](Zcard(key, Return(_)))

  def zcount[F[_]: Functor](key: String, min: Endpoint, max: Endpoint)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Long] =
    inject[F, ZSetAlgebra, Long](Zcount(key, min, max, Return(_)))

  def zincrby[F[_]: Functor](key: String, increment: Double, member: String)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Double] =
    inject[F, ZSetAlgebra, Double](Zincrby(key, increment, member, Return(_)))

  def zinterstore[F[_]: Functor](
    destination: String,
    keys: NonEmptyList[String],
    weights: Option[NonEmptyList[Double]] = None,
    aggregate: Aggregate = Sum)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Double] =
    inject[F, ZSetAlgebra, Double](Zinterstore(destination, keys, weights, aggregate, Return(_)))

  def zrange[F[_]: Functor](
    key: String,
    start: Long,
    stop: Long,
    withScores: Boolean = false)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Seq[(String, Option[Double])]] =
    inject[F, ZSetAlgebra, Seq[(String, Option[Double])]](Zrange(key, start, stop, withScores, Return(_)))

  def zrangebyscore[F[_]: Functor](
    key: String,
    min: Endpoint,
    max: Endpoint,
    withScores: Boolean = false,
    limit: Option[Limit] = None)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Seq[(String, Option[Double])]] =
    inject[F, ZSetAlgebra, Seq[(String, Option[Double])]](Zrangebyscore(key, min, max, withScores, limit, Return(_)))

  def zrank[F[_]: Functor](key: String, member: String)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Option[Long]] =
    inject[F, ZSetAlgebra, Option[Long]](Zrank(key, member, Return(_)))

  def zrem[F[_]: Functor](key: String, members: NonEmptyList[String])(implicit I: Inject[ZSetAlgebra, F]): Free[F, Long] =
    inject[F, ZSetAlgebra, Long](Zrem(key, members, Return(_)))

  def zremrangebyrank[F[_]: Functor](key: String, start: Long, stop: Long)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Long] =
    inject[F, ZSetAlgebra, Long](Zremrangebyrank(key, start, stop, Return(_)))

  def zremrangebyscore[F[_]: Functor](key: String, start: Endpoint, stop: Endpoint)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Long] =
    inject[F, ZSetAlgebra, Long](Zremrangebyscore(key, start, stop, Return(_)))

  def zrevrange[F[_]: Functor](
    key: String,
    start: Long,
    stop: Long,
    withScores: Boolean = false)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Seq[(String, Option[Double])]] =
    inject[F, ZSetAlgebra, Seq[(String, Option[Double])]](Zrevrange(key, start, stop, withScores, Return(_)))

  def zrevrangebyscore[F[_]: Functor](
    key: String,
    min: Endpoint,
    max: Endpoint,
    withScores: Boolean = false,
    limit: Option[Limit] = None)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Seq[(String, Option[Double])]] =
    inject[F, ZSetAlgebra, Seq[(String, Option[Double])]](Zrevrangebyscore(key, min, max, withScores, limit, Return(_)))

  def zrevrank[F[_]: Functor](key: String, member: String)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Option[Long]] =
    inject[F, ZSetAlgebra, Option[Long]](Zrevrank(key, member, Return(_)))

  def zscore[F[_]: Functor](key: String, member: String)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Option[Double]] =
    inject[F, ZSetAlgebra, Option[Double]](Zscore(key, member, Return(_)))

  def zunionstore[F[_]: Functor](
    destination: String,
    keys: NonEmptyList[String],
    weights: Option[NonEmptyList[Double]] = None,
    aggregate: Aggregate = Sum)(implicit I: Inject[ZSetAlgebra, F]): Free[F, Double] =
    inject[F, ZSetAlgebra, Double](Zunionstore(destination, keys, weights, aggregate, Return(_)))
}

object ZSetAlgebra extends ZSetTypes with ZSetInstances with ZSetFunctions
