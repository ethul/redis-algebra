package redis
package algebra

import scalaz.{\/, Free, Functor}, Free.{Gosub, Return, Suspend}

import StringAlgebra._

trait StringAlgebra[A] extends RedisAlgebra[A]

final case class Append[A](key: String, value: String, h: Int => A) extends StringAlgebra[A]

final case class Bitcount[A](key: String, start: Option[Int], end: Option[Int], h: Int => A) extends StringAlgebra[A]

final case class Bitop[A](operation: BitOperation, h: Int => A) extends StringAlgebra[A]

final case class Decr[A](key: String, h: Int => A) extends StringAlgebra[A]

final case class Decrby[A](key: String, decrement: Int, h: Int => A) extends StringAlgebra[A]

final case class Get[A](key: String, h: Option[String] => A) extends StringAlgebra[A]

final case class Getbit[A](key: String, offset: Int, h: Int => A) extends StringAlgebra[A]

final case class Getrange[A](key: String, start: Int, end: Int, h: String => A) extends StringAlgebra[A]

final case class Getset[A](key: String, value: String, h: Option[String] => A) extends StringAlgebra[A]

final case class Incr[A](key: String, h: Int => A) extends StringAlgebra[A]

final case class Incrby[A](key: String, increment: Int, h: Int => A) extends StringAlgebra[A]

final case class Incrbyfloat[A](key: String, increment: Float, h: Float => A) extends StringAlgebra[A]

final case class Mget[A](keys: Seq[String], h: Seq[Option[String]] => A) extends StringAlgebra[A]

final case class Mset[A](pairs: Seq[(String, String)], a: A) extends StringAlgebra[A]

final case class Msetnx[A](pairs: Seq[(String, String)], h: Boolean => A) extends StringAlgebra[A]

final case class Psetex[A](key: String, in: Milliseconds, value: String, a: A) extends StringAlgebra[A]

final case class Set[A](key: String, value: String,
  in: Option[Seconds \/ Milliseconds], option: Option[SetOption], h: Boolean => A) extends StringAlgebra[A]

final case class Setbit[A](key: String, offset: Int, value: String, h: Int => A) extends StringAlgebra[A]

final case class Setex[A](key: String, in: Seconds, value: String, a: A) extends StringAlgebra[A]

final case class Setnx[A](key: String, value: String, h: Boolean => A) extends StringAlgebra[A]

final case class Setrange[A](key: String, offset: Int, value: String, h: Int => A) extends StringAlgebra[A]

final case class Strlen[A](key: String, h: Int => A) extends StringAlgebra[A]

sealed trait BitOperation
final case class And(dest: String, keys: Seq[String]) extends BitOperation
final case class Or(dest: String, keys: Seq[String]) extends BitOperation
final case class Xor(dest: String, keys: Seq[String]) extends BitOperation
final case class Not(dest: String, key: String) extends BitOperation

sealed trait SetOption
case object Nx extends SetOption
case object Xx extends SetOption

sealed trait StringInstances {
  implicit def stringAlgebraFunctor: Functor[StringAlgebra] =
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
  def append(key: String, value: String): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Append(key, value, Return(_)))

  def bitcount(key: String, start: Option[Int] = None, end: Option[Int] = None): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Bitcount(key, start, end, Return(_)))

  def bitop(operation: BitOperation): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Bitop(operation, Return(_)))

  def decr(key: String): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Decr(key, Return(_)))

  def decrby(key: String, decrement: Int): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Decrby(key, decrement, Return(_)))

  def get(key: String): Free[RedisAlgebra, Option[String]] =
    Suspend[RedisAlgebra, Option[String]](Get(key, Return(_)))

  def getbit(key: String, offset: Int): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Getbit(key, offset, Return(_)))

  def getrange(key: String, start: Int, end: Int): Free[RedisAlgebra, String] =
    Suspend[RedisAlgebra, String](Getrange(key, start, end, Return(_)))

  def getset(key: String, value: String): Free[RedisAlgebra, Option[String]] =
    Suspend[RedisAlgebra, Option[String]](Getset(key, value, Return(_)))

  def incr(key: String): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Incr(key, Return(_)))

  def incrby(key: String, increment: Int): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Incrby(key, increment, Return(_)))

  def incrbyfloat(key: String, increment: Float): Free[RedisAlgebra, Float] =
    Suspend[RedisAlgebra, Float](Incrbyfloat(key, increment, Return(_)))

  def mget(keys: Seq[String]): Free[RedisAlgebra, Seq[Option[String]]] =
    Suspend[RedisAlgebra, Seq[Option[String]]](Mget(keys, Return(_)))

  def mset(pairs: Seq[(String, String)]): Free[RedisAlgebra, Unit] =
    Suspend[RedisAlgebra, Unit](Mset(pairs, Return(())))

  def msetnx(pairs: Seq[(String, String)]): Free[RedisAlgebra, Boolean] =
    Suspend[RedisAlgebra, Boolean](Msetnx(pairs, Return(_)))

  def psetex(key: String, in: Milliseconds, value: String): Free[RedisAlgebra, Unit] =
    Suspend[RedisAlgebra, Unit](Psetex(key, in, value, Return(())))

  def set(key: String, value: String,
    in: Option[Seconds \/ Milliseconds] = None, option: Option[SetOption] = None): Free[RedisAlgebra, Boolean] =
    Suspend[RedisAlgebra, Boolean](Set(key, value, in, option, Return(_)))

  def setbit(key: String, offset: Int, value: String): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Setbit(key, offset, value, Return(_)))

  def setex(key: String, in: Seconds, value: String): Free[RedisAlgebra, Unit] =
    Suspend[RedisAlgebra, Unit](Setex(key, in, value, Return(())))

  def setnx(key: String, value: String): Free[RedisAlgebra, Boolean] =
    Suspend[RedisAlgebra, Boolean](Setnx(key, value, Return(_)))

  def setrange(key: String, offset: Int, value: String): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Setrange(key, offset, value, Return(_)))

  def strlen(key: String): Free[RedisAlgebra, Int] =
    Suspend[RedisAlgebra, Int](Strlen(key, Return(_)))
}

object StringAlgebra extends StringInstances with StringFunctions
