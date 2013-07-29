package redis
package algebra

import scalaz.{Free, Functor, NonEmptyList}, Free.{Gosub, Return, Suspend}

import ZSetAlgebra._

sealed trait ZSetAlgebra[A] extends RedisAlgebra[A]

final case class Zadd[A](key: String, pairs: NonEmptyList[(Int, String)], h: Int => A) extends ZSetAlgebra[A]

final case class Zcard[A](key: String, h: Int => A) extends ZSetAlgebra[A]

final case class Zcount[A](key: String, min: Endpoint, max: Endpoint, h: Int => A) extends ZSetAlgebra[A]

final case class Zincrby[A](key: String, increment: Double, member: String, h: Double => A) extends ZSetAlgebra[A]

final case class Zinterstore[A](
  destination: String, numKeys: Int, keys: NonEmptyList[String],
  weights: Option[NonEmptyList[Double]], aggregate: Aggregate, h: Int => A) extends ZSetAlgebra[A]

final case class Zrange[A](
  key: String, start: Int, stop: Int,
  withScores: Boolean, h: Seq[(String, Option[Double])] => A) extends ZSetAlgebra[A]

final case class Zrangebyscore[A](
  key: String, min: Endpoint, max: Endpoint,
  withScores: Boolean, limit: Option[Limit], h: Seq[(String, Option[Double])] => A) extends ZSetAlgebra[A]

final case class Zrank[A](key: String, member: String, h: Option[Int] => A) extends ZSetAlgebra[A]

final case class Zrem[A](key: String, members: NonEmptyList[String], h: Int => A) extends ZSetAlgebra[A]

final case class Zremrangebyrank[A](key: String, start: Int, stop: Int, h: Int => A) extends ZSetAlgebra[A]

final case class Zremrangebyscore[A](key: String, start: Endpoint, stop: Endpoint, h: Int => A) extends ZSetAlgebra[A]

final case class Zrevrange[A](
  key: String, start: Int, stop: Int,
  withScores: Boolean, h: Seq[(String, Option[Double])] => A) extends ZSetAlgebra[A]

final case class Zrevrangebyscore[A](
  key: String, min: Endpoint, max: Endpoint,
  withScores: Boolean, limit: Option[Limit], h: Seq[(String, Option[Double])] => A) extends ZSetAlgebra[A]

final case class Zrevrank[A](key: String, member: String, h: Option[Int] => A) extends ZSetAlgebra[A]

final case class Zscore[A](key: String, member: String, h: Option[Double] => A) extends ZSetAlgebra[A]

final case class Zunionstore[A](
  destination: String, numKeys: Int, keys: NonEmptyList[String],
  weights: Option[NonEmptyList[Double]], aggregate: Aggregate, h: Int => A) extends ZSetAlgebra[A]

sealed trait Endpoint
final case class Closed(value: Double) extends Endpoint
final case class Open(value: Double) extends Endpoint
case object -∞ extends Endpoint
case object +∞ extends Endpoint

sealed trait Aggregate
case object Sum extends Aggregate
case object Min extends Aggregate
case object Max extends Aggregate

final case class Limit(offset: Int, count: Int)

sealed trait ZSetInstances {
  implicit def zsetAlgebraFunctor: Functor[ZSetAlgebra] =
    new Functor[ZSetAlgebra] {
      def map[A, B](a: ZSetAlgebra[A])(f: A => B): ZSetAlgebra[B] = a match {
        case Zadd(k, p, h) => Zadd(k, p, x => f(h(x)))
        case Zcard(k, h) => Zcard(k, x => f(h(x)))
        case Zcount(k, m, n, h) => Zcount(k, m, n, x => f(h(x)))
        case Zincrby(k, i, m, h) => Zincrby(k, i, m, x => f(h(x)))
        case Zinterstore(d, n, k, w, g, h) => Zinterstore(d, n, k, w, g, x => f(h(x)))
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
        case Zunionstore(d, n, k, w, g, h) => Zunionstore(d, n, k, w, g, x => f(h(x)))
      }
    }
}

sealed trait ZSetFunctions {
  def zadd(key: String, pairs: NonEmptyList[(Int, String)]): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Zadd(key, pairs, Return(_)))

  def zcard(key: String): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Zcard(key, Return(_)))

  def zcount(key: String, min: Endpoint, max: Endpoint): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Zcount(key, min, max, Return(_)))

  def zincrby(key: String, increment: Double, member: String): Free[RedisAlgebra, Double] =
    Suspend[RedisAlgebra, Double](Zincrby(key, increment, member, Return(_)))

  def zinterstore(
    destination: String, numKeys: Int, keys: NonEmptyList[String],
    weights: Option[NonEmptyList[Double]] = None, aggregate: Aggregate = Sum): Free[RedisAlgebra, Double] =
    Suspend[RedisAlgebra, Double](Zinterstore(destination, numKeys, keys, weights, aggregate, Return(_)))

  def zrange(
    key: String, start: Int, stop: Int,
    withScores: Boolean = false): Free[RedisAlgebra, Seq[(String, Option[Double])]] =
    Suspend[RedisAlgebra, Seq[(String, Option[Double])]](Zrange(key, start, stop, withScores, Return(_)))

  def zrangebyscore(
    key: String, min: Endpoint, max: Endpoint,
    withScores: Boolean = false, limit: Option[Limit] = None): Free[RedisAlgebra, Seq[(String, Option[Double])]] =
    Suspend[RedisAlgebra, Seq[(String, Option[Double])]](Zrangebyscore(key, min, max, withScores, limit, Return(_)))

  def zrank(key: String, member: String): Free[RedisAlgebra, Option[Int]] =
    Suspend[RedisAlgebra, Option[Int]](Zrank(key, member, Return(_)))

  def zrem(key: String, members: NonEmptyList[String]): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Zrem(key, members, Return(_)))

  def zremrangebyrank(key: String, start: Int, stop: Int): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Zremrangebyrank(key, start, stop, Return(_)))

  def zremrangebyscore(key: String, start: Endpoint, stop: Endpoint): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Zremrangebyscore(key, start, stop, Return(_)))

  def zrevrange(
    key: String, start: Int, stop: Int,
    withScores: Boolean = false): Free[RedisAlgebra, Seq[(String, Option[Double])]] =
    Suspend[RedisAlgebra, Seq[(String, Option[Double])]](Zrevrange(key, start, stop, withScores, Return(_)))

  def zrevrangebyscore(
    key: String, min: Endpoint, max: Endpoint,
    withScores: Boolean = false, limit: Option[Limit] = None): Free[RedisAlgebra, Seq[(String, Option[Double])]] =
    Suspend[RedisAlgebra, Seq[(String, Option[Double])]](Zrevrangebyscore(key, min, max, withScores, limit, Return(_)))

  def zrevrank(key: String, member: String): Free[RedisAlgebra, Option[Int]] =
    Suspend[RedisAlgebra, Option[Int]](Zrevrank(key, member, Return(_)))

  def zscore(key: String, member: String): Free[RedisAlgebra, Option[Double]] =
    Suspend[RedisAlgebra, Option[Double]](Zscore(key, member, Return(_)))

  def zunionstore(
    destination: String, numKeys: Int, keys: NonEmptyList[String],
    weights: Option[NonEmptyList[Double]] = None, aggregate: Aggregate = Sum): Free[RedisAlgebra, Double] =
    Suspend[RedisAlgebra, Double](Zunionstore(destination, numKeys, keys, weights, aggregate, Return(_)))
}

object ZSetAlgebra extends ZSetInstances with ZSetFunctions
