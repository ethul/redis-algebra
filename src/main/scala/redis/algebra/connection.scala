package redis
package algebra

import scalaz.{Free, Functor, Inject, InjectFunctions}, Free.Return

sealed trait ConnectionAlgebra[A]

final case class Auth[A](password: String, h: Status => A) extends ConnectionAlgebra[A]

final case class Echo[A](message: String, h: String => A) extends ConnectionAlgebra[A]

final case class Ping[A](h: Status => A) extends ConnectionAlgebra[A]

final case class Quit[A](h: Status => A) extends ConnectionAlgebra[A]

final case class Select[A](index: Short, h: Status => A) extends ConnectionAlgebra[A]

trait ConnectionInstances {
  implicit val connectionAlgebraFunctor: Functor[ConnectionAlgebra] =
    new Functor[ConnectionAlgebra] {
      def map[A, B](a: ConnectionAlgebra[A])(f: A => B): ConnectionAlgebra[B] =
        a match {
          case Auth(p, h) => Auth(p, x => f(h(x)))
          case Echo(m, h) => Echo(m, x => f(h(x)))
          case Ping(h) => Ping(x => f(h(x)))
          case Quit(h) => Quit(x => f(h(x)))
          case Select(i, h) => Select(i, x => f(h(x)))
        }
    }
}

trait ConnectionFunctions extends InjectFunctions {
  def auth[F[_]: Functor](password: String)(implicit I: Inject[ConnectionAlgebra, F]): Free[F, Status] =
    inject[F, ConnectionAlgebra, Status](Auth(password, Return(_)))

  def echo[F[_]: Functor](message: String)(implicit I: Inject[ConnectionAlgebra, F]): Free[F, String] =
    inject[F, ConnectionAlgebra, String](Echo(message, Return(_)))

  def ping[F[_]: Functor](implicit I: Inject[ConnectionAlgebra, F]): Free[F, Status] =
    inject[F, ConnectionAlgebra, Status](Ping(Return(_)))

  def quit[F[_]: Functor](implicit I: Inject[ConnectionAlgebra, F]): Free[F, Status] =
    inject[F, ConnectionAlgebra, Status](Quit(Return(_)))

  def select[F[_]: Functor](index: Short)(implicit I: Inject[ConnectionAlgebra, F]): Free[F, Status] =
    inject[F, ConnectionAlgebra, Status](Select(index, Return(_)))
}

sealed abstract class Status
case object Ok extends Status
case object Error extends Status
case object Wrongtype extends Status
