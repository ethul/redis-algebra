package redis
package algebra

import scalaz.{Free, Functor, Inject, InjectFunctions, NonEmptyList}

import data.{Asc, Position, Status}

sealed abstract class ListAlgebra[A]

final case class Blpop[A](keys: NonEmptyList[ByteString], timeout: Seconds, h: Option[(ByteString, ByteString)] => A) extends ListAlgebra[A]

final case class Brpop[A](keys: NonEmptyList[ByteString], timeout: Seconds, h: Option[(ByteString, ByteString)] => A) extends ListAlgebra[A]

final case class Brpoplpush[A](source: ByteString, destination: ByteString, timeout: Seconds, h: Option[ByteString] => A) extends ListAlgebra[A]

final case class Lindex[A](key: ByteString, index: Long, h: Option[ByteString] => A) extends ListAlgebra[A]

final case class Linsert[A](key: ByteString, position: Position, pivot: ByteString, value: ByteString, h: Option[Long] => A) extends ListAlgebra[A]

final case class Llen[A](key: ByteString, h: Long => A) extends ListAlgebra[A]

final case class Lpop[A](key: ByteString, h: Option[ByteString] => A) extends ListAlgebra[A]

final case class Lpush[A](key: ByteString, values: NonEmptyList[ByteString], h: Long => A) extends ListAlgebra[A]

final case class Lpushx[A](key: ByteString, value: ByteString, h: Long => A) extends ListAlgebra[A]

final case class Lrange[A](key: ByteString, start: Long, stop: Long, h: Seq[ByteString] => A) extends ListAlgebra[A]

final case class Lrem[A](key: ByteString, count: Long, value: ByteString, h: Long => A) extends ListAlgebra[A]

final case class Lset[A](key: ByteString, index: Long, value: ByteString, h: Status => A) extends ListAlgebra[A]

final case class Ltrim[A](key: ByteString, start: Long, stop: Long, a: Status => A) extends ListAlgebra[A]

final case class Rpop[A](key: ByteString, h: Option[ByteString] => A) extends ListAlgebra[A]

final case class Rpoplpush[A](source: ByteString, destination: ByteString, h: Option[ByteString] => A) extends ListAlgebra[A]

final case class Rpush[A](key: ByteString, values: NonEmptyList[ByteString], h: Long => A) extends ListAlgebra[A]

final case class Rpushx[A](key: ByteString, value: ByteString, h: Long => A) extends ListAlgebra[A]

trait ListInstances {
  implicit val listAlgebraFunctor: Functor[ListAlgebra] =
    new Functor[ListAlgebra] {
      def map[A, B](a: ListAlgebra[A])(f: A => B): ListAlgebra[B] =
        a match {
          case Blpop(k, t, h) => Blpop(k, t, x => f(h(x)))
          case Brpop(k, t, h) => Brpop(k, t, x => f(h(x)))
          case Brpoplpush(s, d, t, h) => Brpoplpush(s, d, t, x => f(h(x)))
          case Lindex(k, i, h) => Lindex(k, i, x => f(h(x)))
          case Linsert(k, p, i, v, h) => Linsert(k, p, i, v, x => f(h(x)))
          case Llen(k, h) => Llen(k, x => f(h(x)))
          case Lpop(k, h) => Lpop(k, x => f(h(x)))
          case Lpush(k, v, h) => Lpush(k, v, x => f(h(x)))
          case Lpushx(k, v, h) => Lpushx(k, v, x => f(h(x)))
          case Lrange(k, s, t, h) => Lrange(k, s, t, x => f(h(x)))
          case Lrem(k, c, v, h) => Lrem(k, c, v, x => f(h(x)))
          case Lset(k, i, v, h) => Lset(k, i, v, x => f(h(x)))
          case Ltrim(k, s, t, h) => Ltrim(k, s, t, x => f(h(x)))
          case Rpop(k, h) => Rpop(k, x => f(h(x)))
          case Rpoplpush(s, d, h) => Rpoplpush(s, d, x => f(h(x)))
          case Rpush(k, v, h) => Rpush(k, v, x => f(h(x)))
          case Rpushx(k, v, h) => Rpushx(k, v, x => f(h(x)))
        }
    }
}

trait ListFunctions extends InjectFunctions {
  def blpop[F[_]: Functor](keys: NonEmptyList[ByteString], timeout: Seconds)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[(ByteString, ByteString)]] =
    inject[F, ListAlgebra, Option[(ByteString, ByteString)]](Blpop(keys, timeout, Free.point(_)))

  def brpop[F[_]: Functor](keys: NonEmptyList[ByteString], timeout: Seconds)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[(ByteString, ByteString)]] =
    inject[F, ListAlgebra, Option[(ByteString, ByteString)]](Brpop(keys, timeout, Free.point(_)))

  def brpoplpush[F[_]: Functor](source: ByteString, destination: ByteString, timeout: Seconds)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[ByteString]] =
    inject[F, ListAlgebra, Option[ByteString]](Brpoplpush(source, destination, timeout, Free.point(_)))

  def lindex[F[_]: Functor](key: ByteString, index: Long)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[ByteString]] =
    inject[F, ListAlgebra, Option[ByteString]](Lindex(key, index, Free.point(_)))

  def linsert[F[_]: Functor](key: ByteString, position: Position, pivot: ByteString, value: ByteString)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[Long]] =
    inject[F, ListAlgebra, Option[Long]](Linsert(key, position, pivot, value, Free.point(_)))

  def llen[F[_]: Functor](key: ByteString)(implicit I: Inject[ListAlgebra, F]): Free[F, Long] =
    inject[F, ListAlgebra, Long](Llen(key, Free.point(_)))

  def lpop[F[_]: Functor](key: ByteString)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[ByteString]] =
    inject[F, ListAlgebra, Option[ByteString]](Lpop(key, Free.point(_)))

  def lpush[F[_]: Functor](key: ByteString, values: NonEmptyList[ByteString])(implicit I: Inject[ListAlgebra, F]): Free[F, Long] =
    inject[F, ListAlgebra, Long](Lpush(key, values, Free.point(_)))

  def lpushx[F[_]: Functor](key: ByteString, value: ByteString)(implicit I: Inject[ListAlgebra, F]): Free[F, Long] =
    inject[F, ListAlgebra, Long](Lpushx(key, value, Free.point(_)))

  def lrange[F[_]: Functor](key: ByteString, start: Long, stop: Long)(implicit I: Inject[ListAlgebra, F]): Free[F, Seq[ByteString]] =
    inject[F, ListAlgebra, Seq[ByteString]](Lrange(key, start, stop, Free.point(_)))

  def lrem[F[_]: Functor](key: ByteString, count: Long, value: ByteString)(implicit I: Inject[ListAlgebra, F]): Free[F, Long] =
    inject[F, ListAlgebra, Long](Lrem(key, count, value, Free.point(_)))

  def lset[F[_]: Functor](key: ByteString, index: Long, value: ByteString)(implicit I: Inject[ListAlgebra, F]): Free[F, Status] =
    inject[F, ListAlgebra, Status](Lset(key, index, value, Free.point(_)))

  def ltrim[F[_]: Functor](key: ByteString, start: Long, stop: Long)(implicit I: Inject[ListAlgebra, F]): Free[F, Status] =
    inject[F, ListAlgebra, Status](Ltrim(key, start, stop, Free.point(_)))

  def rpop[F[_]: Functor](key: ByteString)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[ByteString]] =
    inject[F, ListAlgebra, Option[ByteString]](Rpop(key, Free.point(_)))

  def rpoplpush[F[_]: Functor](source: ByteString, destination: ByteString)(implicit I: Inject[ListAlgebra, F]): Free[F, Option[ByteString]] =
    inject[F, ListAlgebra, Option[ByteString]](Rpoplpush(source, destination, Free.point(_)))

  def rpush[F[_]: Functor](key: ByteString, values: NonEmptyList[ByteString])(implicit I: Inject[ListAlgebra, F]): Free[F, Long] =
    inject[F, ListAlgebra, Long](Rpush(key, values, Free.point(_)))

  def rpushx[F[_]: Functor](key: ByteString, value: ByteString)(implicit I: Inject[ListAlgebra, F]): Free[F, Long] =
    inject[F, ListAlgebra, Long](Rpushx(key, value, Free.point(_)))
}
