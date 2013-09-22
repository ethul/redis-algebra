package redis
package algebra

import scalaz.{\/, Free, Functor, Inject, InjectFunctions, NonEmptyList}, Free.Return

import data.{LuaResult, Status}

sealed abstract class ScriptAlgebra[A]

final case class Eval[A](script: ByteString, keys: Seq[ByteString], args: Seq[ByteString], h: LuaResult => A) extends ScriptAlgebra[A]

final case class Evalsha[A](sha1: ByteString, keys: Seq[ByteString], args: Seq[ByteString], h: LuaResult => A) extends ScriptAlgebra[A]

final case class Scriptexists[A](scripts: NonEmptyList[ByteString], h: NonEmptyList[Boolean] => A) extends ScriptAlgebra[A]

final case class Scriptflush[A](h: Status => A) extends ScriptAlgebra[A]

final case class Scriptkill[A](h: Status => A) extends ScriptAlgebra[A]

final case class Scriptload[A](script: ByteString, h: Option[ByteString] => A) extends ScriptAlgebra[A]

trait ScriptInstances {
  implicit val scriptAlgebraFunctor: Functor[ScriptAlgebra] =
    new Functor[ScriptAlgebra] {
      def map[A, B](a: ScriptAlgebra[A])(f: A => B): ScriptAlgebra[B] =
        a match {
          case Eval(s, k, a, h) => Eval(s, k, a, x => f(h(x)))
          case Evalsha(s, k, a, h) => Evalsha(s, k, a, x => f(h(x)))
          case Scriptexists(s, h) => Scriptexists(s, x => f(h(x)))
          case Scriptflush(h) => Scriptflush(x => f(h(x)))
          case Scriptkill(h) => Scriptkill(x => f(h(x)))
          case Scriptload(s, h) => Scriptload(s, x => f(h(x)))
        }
    }
}

trait ScriptFunctions extends InjectFunctions {
  def eval[F[_]: Functor](
    script: ByteString,
    keys: Seq[ByteString] = Nil,
    args: Seq[ByteString] = Nil)(implicit I: Inject[ScriptAlgebra, F]): Free[F, LuaResult] =
    inject[F, ScriptAlgebra, LuaResult](Eval(script, keys, args, Return(_)))

  def evalsha[F[_]: Functor](
    sha1: ByteString,
    keys: Seq[ByteString] = Nil,
    args: Seq[ByteString] = Nil)(implicit I: Inject[ScriptAlgebra, F]): Free[F, LuaResult] =
    inject[F, ScriptAlgebra, LuaResult](Evalsha(sha1, keys, args, Return(_)))

  def scriptexists[F[_]: Functor](scripts: NonEmptyList[ByteString])(implicit I: Inject[ScriptAlgebra, F]): Free[F, NonEmptyList[Boolean]] =
    inject[F, ScriptAlgebra, NonEmptyList[Boolean]](Scriptexists(scripts, Return(_)))

  def scriptflush[F[_]: Functor](implicit I: Inject[ScriptAlgebra, F]): Free[F, Status] =
    inject[F, ScriptAlgebra, Status](Scriptflush(Return(_)))

  def scriptkill[F[_]: Functor](implicit I: Inject[ScriptAlgebra, F]): Free[F, Status] =
    inject[F, ScriptAlgebra, Status](Scriptkill(Return(_)))

  def scriptload[F[_]: Functor](script: ByteString)(implicit I: Inject[ScriptAlgebra, F]): Free[F, Option[ByteString]] =
    inject[F, ScriptAlgebra, Option[ByteString]](Scriptload(script, Return(_)))
}
