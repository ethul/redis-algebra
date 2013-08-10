package redis
package algebra

import scalaz.{Free, Functor, NonEmptyList}, Free.{Gosub, Return, Suspend}

import typeclass.Inject, Inject._

import ListAlgebra._

sealed trait ListAlgebra[A]

final case class Blpop[A](keys: NonEmptyList[String], timeout: Seconds, h: Option[(String, String)] => A)
  extends ListAlgebra[A]

final case class Brpop[A](keys: NonEmptyList[String], timeout: Seconds, h: Option[(String, String)] => A)
  extends ListAlgebra[A]

final case class Brpoplpush[A](source: String, destination: String, timeout: Seconds, h: Option[String] => A)
  extends ListAlgebra[A]

final case class Lindex[A](key: String, index: Int, h: Option[String] => A)
  extends ListAlgebra[A]

final case class Linsert[A](key: String, order: Order, pivot: String, value: String, h: Option[Int] => A)
  extends ListAlgebra[A]

final case class Llen[A](key: String, h: Int => A)
  extends ListAlgebra[A]

final case class Lpop[A](key: String, h: Option[String] => A)
  extends ListAlgebra[A]

final case class Lpush[A](key: String, values: NonEmptyList[String], h: Int => A)
  extends ListAlgebra[A]

final case class Lpushx[A](key: String, value: String, h: Int => A)
  extends ListAlgebra[A]

final case class Lrange[A](key: String, start: Int, stop: Int, h: Seq[String] => A)
  extends ListAlgebra[A]

final case class Lrem[A](key: String, count: Int, value: String, h: Int => A)
  extends ListAlgebra[A]

final case class Lset[A](key: String, index: Int, value: String, a: A)
  extends ListAlgebra[A]

final case class Ltrim[A](key: String, start: Int, stop: Int, a: A)
  extends ListAlgebra[A]

final case class Rpop[A](key: String, h: Option[String] => A)
  extends ListAlgebra[A]

final case class Rpoplpush[A](source: String, destination: String, h: Option[String] => A)
  extends ListAlgebra[A]

final case class Rpush[A](key: String, values: NonEmptyList[String], h: Int => A)
  extends ListAlgebra[A]

final case class Rpushx[A](key: String, value: String, h: Int => A)
  extends ListAlgebra[A]

sealed trait Order
case object Before extends Order
case object After extends Order

sealed trait ListInstances {
  implicit def listAlgebraFunctor: Functor[ListAlgebra] =
    new Functor[ListAlgebra] {
      def map[A, B](a: ListAlgebra[A])(f: A => B): ListAlgebra[B] = a match {
        case Blpop(k, t, h) => Blpop(k, t, x => f(h(x)))
        case Brpop(k, t, h) => Brpop(k, t, x => f(h(x)))
        case Brpoplpush(s, d, t, h) => Brpoplpush(s, d, t, x => f(h(x)))
        case Lindex(k, i, h) => Lindex(k, i, x => f(h(x)))
        case Linsert(k, o, p, v, h) => Linsert(k, o, p, v, x => f(h(x)))
        case Llen(k, h) => Llen(k, x => f(h(x)))
        case Lpop(k, h) => Lpop(k, x => f(h(x)))
        case Lpush(k, v, h) => Lpush(k, v, x => f(h(x)))
        case Lpushx(k, v, h) => Lpushx(k, v, x => f(h(x)))
        case Lrange(k, s, t, h) => Lrange(k, s, t, x => f(h(x)))
        case Lrem(k, c, v, h) => Lrem(k, c, v, x => f(h(x)))
        case Lset(k, i, v, a) => Lset(k, i, v, f(a))
        case Ltrim(k, s, t, a) => Ltrim(k, s, t, f(a))
        case Rpop(k, h) => Rpop(k, x => f(h(x)))
        case Rpoplpush(s, d, h) => Rpoplpush(s, d, x => f(h(x)))
        case Rpush(k, v, h) => Rpush(k, v, x => f(h(x)))
        case Rpushx(k, v, h) => Rpushx(k, v, x => f(h(x)))
      }
    }
}

sealed trait ListFunctions {
  def blpop[F[_]: Functor](keys: NonEmptyList[String], timeout: Seconds)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[(String, String)]] =
    inject[F, ListAlgebra, Option[(String, String)]](Blpop(keys, timeout, Return(_)))

  def brpop[F[_]: Functor](keys: NonEmptyList[String], timeout: Seconds)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[(String, String)]] =
    inject[F, ListAlgebra, Option[(String, String)]](Brpop(keys, timeout, Return(_)))

  def brpoplpush[F[_]: Functor](source: String, destination: String, timeout: Seconds)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[String]] =
    inject[F, ListAlgebra, Option[String]](Brpoplpush(source, destination, timeout, Return(_)))

  def lindex[F[_]: Functor](key: String, index: Int)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[String]] =
    inject[F, ListAlgebra, Option[String]](Lindex(key, index, Return(_)))

  def linsert[F[_]: Functor](key: String, order: Order, pivot: String, value: String)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[Int]] =
    inject[F, ListAlgebra, Option[Int]](Linsert(key, order, pivot, value, Return(_)))

  def llen[F[_]: Functor](key: String)(implicit I: Inject[ListAlgebra, F]): Free[F, Int] =
    inject[F, ListAlgebra, Int](Llen(key, Return(_)))

  def lpop[F[_]: Functor](key: String)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[String]] =
    inject[F, ListAlgebra, Option[String]](Lpop(key, Return(_)))

  def lpush[F[_]: Functor](key: String, values: NonEmptyList[String])(implicit I: Inject[ListAlgebra, F]): Free[F, Int] =
    inject[F, ListAlgebra, Int](Lpush(key, values, Return(_)))

  def lpushx[F[_]: Functor](key: String, value: String)(implicit I: Inject[ListAlgebra, F]): Free[F, Int] =
    inject[F, ListAlgebra, Int](Lpushx(key, value, Return(_)))

  def lrange[F[_]: Functor](key: String, start: Int, stop: Int)(implicit I: Inject[ListAlgebra, F]): Free[F, Seq[String]] =
    inject[F, ListAlgebra, Seq[String]](Lrange(key, start, stop, Return(_)))

  def lrem[F[_]: Functor](key: String, count: Int, value: String)(implicit I: Inject[ListAlgebra, F]): Free[F, Int] =
    inject[F, ListAlgebra, Int](Lrem(key, count, value, Return(_)))

  def lset[F[_]: Functor](key: String, index: Int, value: String)(implicit I: Inject[ListAlgebra, F]): Free[F, Unit] =
    inject[F, ListAlgebra, Unit](Lset(key, index, value, Return(())))

  def ltrim[F[_]: Functor](key: String, start: Int, stop: Int)(implicit I: Inject[ListAlgebra, F]): Free[F, Unit] =
    inject[F, ListAlgebra, Unit](Ltrim(key, start, stop, Return(())))

  def rpop[F[_]: Functor](key: String)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[String]] =
    inject[F, ListAlgebra, Option[String]](Rpop(key, Return(_)))

  def rpoplpush[F[_]: Functor](source: String, destination: String)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[String]] =
    inject[F, ListAlgebra, Option[String]](Rpoplpush(source, destination, Return(_)))

  def rpush[F[_]: Functor](key: String, values: NonEmptyList[String])(implicit I: Inject[ListAlgebra, F]): Free[F, Int] =
    inject[F, ListAlgebra, Int](Rpush(key, values, Return(_)))

  def rpushx[F[_]: Functor](key: String, value: String)(implicit I: Inject[ListAlgebra, F]): Free[F, Int] =
    inject[F, ListAlgebra, Int](Rpushx(key, value, Return(_)))
}

object ListAlgebra extends ListInstances with ListFunctions
