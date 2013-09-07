package redis
package algebra

import scalaz.{\/, Free, Functor}, Free.Return

import typeclass.Inject, Inject._

import ConnectionAlgebra._

sealed trait ConnectionAlgebra[A]

final case class Auth[A](password: String, a: A) extends ConnectionAlgebra[A]

final case class Echo[A](message: String, h: String => A) extends ConnectionAlgebra[A]

final case class Ping[A](h: Option[pong.type] => A) extends ConnectionAlgebra[A]

final case class Quit[A](a: A) extends ConnectionAlgebra[A]

final case class Select[A](index: Short, a: A) extends ConnectionAlgebra[A]

case object pong

sealed trait ConnectionInstances {
  implicit val connectionAlgebraFunctor: Functor[ConnectionAlgebra] =
    new Functor[ConnectionAlgebra] {
      def map[A, B](a: ConnectionAlgebra[A])(f: A => B): ConnectionAlgebra[B] =
        a match {
          case Auth(p, a) => Auth(p, f(a))
          case Echo(m, h) => Echo(m, x => f(h(x)))
          case Ping(h) => Ping(x => f(h(x)))
          case Quit(a) => Quit(f(a))
          case Select(i, a) => Select(i, f(a))
        }
    }
}

sealed trait ConnectionFunctions {
  def auth[F[_]: Functor](password: String)(implicit I: Inject[ConnectionAlgebra, F]): Free[F, Unit] =
    inject[F, ConnectionAlgebra, Unit](Auth(password, Return(())))

  def echo[F[_]: Functor](message: String)(implicit I: Inject[ConnectionAlgebra, F]): Free[F, String] =
    inject[F, ConnectionAlgebra, String](Echo(message, Return(_)))

  def ping[F[_]: Functor](implicit I: Inject[ConnectionAlgebra, F]): Free[F, Option[pong.type]] =
    inject[F, ConnectionAlgebra, Option[pong.type]](Ping(Return(_)))

  def quit[F[_]: Functor](implicit I: Inject[ConnectionAlgebra, F]): Free[F, Unit] =
    inject[F, ConnectionAlgebra, Unit](Quit(Return(())))

  def select[F[_]: Functor](index: Short)(implicit I: Inject[ConnectionAlgebra, F]): Free[F, Unit] =
    inject[F, ConnectionAlgebra, Unit](Select(index, Return(())))
}

object ConnectionAlgebra extends ConnectionInstances with ConnectionFunctions
