package redis
package algebra

import scalaz.{\/, Free, Functor, NonEmptyList}, Free.{Gosub, Return, Suspend}

import HashAlgebra._

sealed trait HashAlgebra[A] extends RedisAlgebra[A]

final case class Hdel[A](key: String, fields: NonEmptyList[String], h: Int => A) extends HashAlgebra[A]

final case class Hexists[A](key: String, field: String, h: Boolean => A) extends HashAlgebra[A]

final case class Hget[A](key: String, field: String, h: Option[String] => A) extends HashAlgebra[A]

final case class Hgetall[A](key: String, h: Seq[(String, String)] => A) extends HashAlgebra[A]

final case class Hincby[A](key: String, field: String, increment: Int, h: Int => A) extends HashAlgebra[A]

final case class Hincbyfloat[A](key: String, field: String, increment: Float, h: Float => A) extends HashAlgebra[A]

final case class Hkeys[A](key: String, h: Seq[String] => A) extends HashAlgebra[A]

final case class Hlen[A](key: String, h: Int => A) extends HashAlgebra[A]

final case class Hmget[A](key: String, fields: NonEmptyList[String], h: Seq[Option[String]] => A) extends HashAlgebra[A]

final case class Hmset[A](key: String, pairs: NonEmptyList[(String, String)], a: A) extends HashAlgebra[A]

final case class Hset[A](key: String, field: String, value: String, h: Boolean => A) extends HashAlgebra[A]

final case class Hsetnx[A](key: String, field: String, value: String, h: Boolean => A) extends HashAlgebra[A]

final case class Hvals[A](key: String, h: Seq[String] => A) extends HashAlgebra[A]

sealed trait HashInstances {
  implicit def hashAlgebraFunctor: Functor[HashAlgebra] =
    new Functor[HashAlgebra] {
      def map[A, B](a: HashAlgebra[A])(f: A => B): HashAlgebra[B] = a match {
        case Hdel(k, s, h) => Hdel(k, s, x => f(h(x)))
        case Hexists(k, s, h) => Hexists(k, s, x => f(h(x)))
        case Hget(k, s, h) => Hget(k, s, x => f(h(x)))
        case Hgetall(k, h) => Hgetall(k, x => f(h(x)))
        case Hincby(k, s, i, h) => Hincby(k, s, i, x => f(h(x)))
        case Hincbyfloat(k, s, i, h) => Hincbyfloat(k, s, i, x => f(h(x)))
        case Hkeys(k, h) => Hkeys(k, x => f(h(x)))
        case Hlen(k, h) => Hlen(k, x => f(h(x)))
        case Hmget(k, s, h) => Hmget(k, s, x => f(h(x)))
        case Hmset(k, p, a) => Hmset(k, p, f(a))
        case Hset(k, s, v, h) => Hset(k, s, v, x => f(h(x)))
        case Hsetnx(k, s, v, h) => Hsetnx(k, s, v, x => f(h(x)))
        case Hvals(k, h) => Hvals(k, x => f(h(x)))
      }
    }
}

sealed trait HashFunctions {
  def hdel(key: String, fields: NonEmptyList[String]): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Hdel(key, fields, Return(_)))

  def hexists(key: String, field: String): Free[RedisAlgebra, Boolean] =
    Suspend[RedisAlgebra, Boolean](Hexists(key, field, Return(_)))

  def hget(key: String, field: String): Free[RedisAlgebra, Option[String]] =
    Suspend[RedisAlgebra, Option[String]](Hget(key, field, Return(_)))

  def hgetall(key: String): Free[RedisAlgebra, Seq[(String, String)]] =
    Suspend[RedisAlgebra, Seq[(String, String)]](Hgetall(key, Return(_)))

  def hincby(key: String, field: String, increment: Int): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Hincby(key, field, increment, Return(_)))

  def hincbyfloat(key: String, field: String, increment: Float): Free[RedisAlgebra, Float] =
    Suspend[RedisAlgebra, Float](Hincbyfloat(key, field, increment, Return(_)))

  def hkeys(key: String): Free[RedisAlgebra, Seq[String]] =
    Suspend[RedisAlgebra, Seq[String]](Hkeys(key, Return(_)))

  def hlen(key: String): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Hlen(key, Return(_)))

  def hmget(key: String, fields: NonEmptyList[String]): Free[RedisAlgebra, Seq[Option[String]]] =
    Suspend[RedisAlgebra, Seq[Option[String]]](Hmget(key, fields, Return(_)))

  def hmset(key: String, pairs: NonEmptyList[(String, String)]): Free[RedisAlgebra, Unit] =
    Suspend[RedisAlgebra, Unit](Hmset(key, pairs, Return(())))

  def hset(key: String, field: String, value: String): Free[RedisAlgebra, Boolean] =
    Suspend[RedisAlgebra, Boolean](Hset(key, field, value, Return(_)))

  def hsetnx(key: String, field: String, value: String): Free[RedisAlgebra, Boolean] =
    Suspend[RedisAlgebra, Boolean](Hsetnx(key, field, value, Return(_)))

  def hvals(key: String): Free[RedisAlgebra, Seq[String]] =
    Suspend[RedisAlgebra, Seq[String]](Hvals(key, Return(_)))
}

object HashAlgebra extends HashInstances with HashFunctions
