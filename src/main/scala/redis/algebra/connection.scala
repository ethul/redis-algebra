package redis
package algebra

import scalaz.{Free, Functor, Inject, InjectFunctions}

import data.Status

sealed abstract class ConnectionAlgebra[A]

final case class Auth[A](password: ByteString, h: Status => A) extends ConnectionAlgebra[A]

final case class Echo[A](message: ByteString, h: ByteString => A) extends ConnectionAlgebra[A]

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
  def auth[F[_]: Functor](password: ByteString)(implicit I: Inject[ConnectionAlgebra, F]): Free[F, Status] =
    inject[F, ConnectionAlgebra, Status](Auth(password, Free.point(_)))

  def echo[F[_]: Functor](message: ByteString)(implicit I: Inject[ConnectionAlgebra, F]): Free[F, ByteString] =
    inject[F, ConnectionAlgebra, ByteString](Echo(message, Free.point(_)))

  def ping[F[_]: Functor](implicit I: Inject[ConnectionAlgebra, F]): Free[F, Status] =
    inject[F, ConnectionAlgebra, Status](Ping(Free.point(_)))

  def quit[F[_]: Functor](implicit I: Inject[ConnectionAlgebra, F]): Free[F, Status] =
    inject[F, ConnectionAlgebra, Status](Quit(Free.point(_)))

  def select[F[_]: Functor](index: Short)(implicit I: Inject[ConnectionAlgebra, F]): Free[F, Status] =
    inject[F, ConnectionAlgebra, Status](Select(index, Free.point(_)))
}
