package redis
package algebra

import scalaz.{Free, Functor, Inject, InjectFunctions, NonEmptyList}, Free.Return

sealed trait HashAlgebra[A]

final case class Hdel[A](key: String, fields: NonEmptyList[String], h: Long => A) extends HashAlgebra[A]

final case class Hexists[A](key: String, field: String, h: Boolean => A) extends HashAlgebra[A]

final case class Hget[A](key: String, field: String, h: Option[String] => A) extends HashAlgebra[A]

final case class Hgetall[A](key: String, h: Seq[(String, String)] => A) extends HashAlgebra[A]

final case class Hincrby[A](key: String, field: String, increment: Long, h: Long=> A) extends HashAlgebra[A]

final case class Hincrbyfloat[A](key: String, field: String, increment: Float, h: Float => A) extends HashAlgebra[A]

final case class Hkeys[A](key: String, h: Seq[String] => A) extends HashAlgebra[A]

final case class Hlen[A](key: String, h: Long => A) extends HashAlgebra[A]

final case class Hmget[A](key: String, fields: NonEmptyList[String], h: Seq[Option[String]] => A) extends HashAlgebra[A]

final case class Hmset[A](key: String, pairs: NonEmptyList[(String, String)], a: A) extends HashAlgebra[A]

final case class Hset[A](key: String, field: String, value: String, h: Boolean => A) extends HashAlgebra[A]

final case class Hsetnx[A](key: String, field: String, value: String, h: Boolean => A) extends HashAlgebra[A]

final case class Hvals[A](key: String, h: Seq[String] => A) extends HashAlgebra[A]

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
          case Hmset(k, p, a) => Hmset(k, p, f(a))
          case Hset(k, s, v, h) => Hset(k, s, v, x => f(h(x)))
          case Hsetnx(k, s, v, h) => Hsetnx(k, s, v, x => f(h(x)))
          case Hvals(k, h) => Hvals(k, x => f(h(x)))
        }
    }
}

trait HashFunctions extends InjectFunctions {
  def hdel[F[_]: Functor](key: String, fields: NonEmptyList[String])(implicit I: Inject[HashAlgebra, F]): Free[F, Long] =
    inject[F, HashAlgebra, Long](Hdel(key, fields, Return(_)))

  def hexists[F[_]: Functor](key: String, field: String)(implicit I: Inject[HashAlgebra, F]): Free[F, Boolean] =
    inject[F, HashAlgebra, Boolean](Hexists(key, field, Return(_)))

  def hget[F[_]: Functor](key: String, field: String)(implicit I: Inject[HashAlgebra, F]): Free[F, Option[String]] =
    inject[F, HashAlgebra, Option[String]](Hget(key, field, Return(_)))

  def hgetall[F[_]: Functor](key: String)(implicit I: Inject[HashAlgebra, F]): Free[F, Seq[(String, String)]] =
    inject[F, HashAlgebra, Seq[(String, String)]](Hgetall(key, Return(_)))

  def hincrby[F[_]: Functor](key: String, field: String, increment: Long)(implicit I: Inject[HashAlgebra, F]): Free[F, Long] =
    inject[F, HashAlgebra, Long](Hincrby(key, field, increment, Return(_)))

  def hincrbyfloat[F[_]: Functor](key: String, field: String, increment: Float)(implicit I: Inject[HashAlgebra, F]): Free[F, Float] =
    inject[F, HashAlgebra, Float](Hincrbyfloat(key, field, increment, Return(_)))

  def hkeys[F[_]: Functor](key: String)(implicit I: Inject[HashAlgebra, F]): Free[F, Seq[String]] =
    inject[F, HashAlgebra, Seq[String]](Hkeys(key, Return(_)))

  def hlen[F[_]: Functor](key: String)(implicit I: Inject[HashAlgebra, F]): Free[F, Long] =
    inject[F, HashAlgebra, Long](Hlen(key, Return(_)))

  def hmget[F[_]: Functor](key: String, fields: NonEmptyList[String])(implicit I: Inject[HashAlgebra, F]): Free[F, Seq[Option[String]]] =
    inject[F, HashAlgebra, Seq[Option[String]]](Hmget(key, fields, Return(_)))

  def hmset[F[_]: Functor](key: String, pairs: NonEmptyList[(String, String)])(implicit I: Inject[HashAlgebra, F]): Free[F, Unit] =
    inject[F, HashAlgebra, Unit](Hmset(key, pairs, Return(())))

  def hset[F[_]: Functor](key: String, field: String, value: String)(implicit I: Inject[HashAlgebra, F]): Free[F, Boolean] =
    inject[F, HashAlgebra, Boolean](Hset(key, field, value, Return(_)))

  def hsetnx[F[_]: Functor](key: String, field: String, value: String)(implicit I: Inject[HashAlgebra, F]): Free[F, Boolean] =
    inject[F, HashAlgebra, Boolean](Hsetnx(key, field, value, Return(_)))

  def hvals[F[_]: Functor](key: String)(implicit I: Inject[HashAlgebra, F]): Free[F, Seq[String]] =
    inject[F, HashAlgebra, Seq[String]](Hvals(key, Return(_)))
}
