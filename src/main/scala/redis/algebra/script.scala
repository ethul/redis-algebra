package redis
package algebra

import scalaz.{\/, Free, Functor, NonEmptyList}, Free.Return

import typeclass.Inject, Inject._

import ScriptAlgebra._

sealed trait ScriptAlgebra[A]

final case class Eval[A](script: String, keys: Seq[String], args: Seq[String], h: LuaError \/ LuaValue => A) extends ScriptAlgebra[A]

final case class Evalsha[A](sha1: String, keys: Seq[String], args: Seq[String], h: LuaError \/ LuaValue => A) extends ScriptAlgebra[A]

final case class Scriptexists[A](scripts: NonEmptyList[String], h: NonEmptyList[Boolean] => A) extends ScriptAlgebra[A]

final case class Scriptflush[A](a: A) extends ScriptAlgebra[A]

final case class Scriptkill[A](a: A) extends ScriptAlgebra[A]

final case class Scriptload[A](script: String, h: String => A) extends ScriptAlgebra[A]

sealed trait ScriptTypes {
  case class LuaError(value: String)

  sealed trait LuaValue
  case class LuaNumber(value: Long) extends LuaValue
  case class LuaString(value: String) extends LuaValue
  case class LuaBoolean(value: Boolean) extends LuaValue
  case class LuaTable(value: Map[LuaValue, LuaValue]) extends LuaValue
  case object LuaUnit extends LuaValue
}

sealed trait ScriptInstances {
  implicit val scriptAlgebraFunctor: Functor[ScriptAlgebra] =
    new Functor[ScriptAlgebra] {
      def map[A, B](a: ScriptAlgebra[A])(f: A => B): ScriptAlgebra[B] =
        a match {
          case Eval(s, k, a, h) => Eval(s, k, a, x => f(h(x)))
          case Evalsha(s, k, a, h) => Evalsha(s, k, a, x => f(h(x)))
          case Scriptexists(s, h) => Scriptexists(s, x => f(h(x)))
          case Scriptflush(a) => Scriptflush(f(a))
          case Scriptkill(a) => Scriptkill(f(a))
          case Scriptload(s, h) => Scriptload(s, x => f(h(x)))
        }
    }
}

sealed trait ScriptFunctions {
  def eval[F[_]: Functor](
    script: String,
    keys: Seq[String] = Nil,
    args: Seq[String] = Nil)(implicit I: Inject[ScriptAlgebra, F]): Free[F, LuaError \/ LuaValue] =
    inject[F, ScriptAlgebra, LuaError \/ LuaValue](Eval(script, keys, args, Return(_)))

  def evalsha[F[_]: Functor](
    sha1: String,
    keys: Seq[String] = Nil,
    args: Seq[String] = Nil)(implicit I: Inject[ScriptAlgebra, F]): Free[F, LuaError \/ LuaValue] =
    inject[F, ScriptAlgebra, LuaError \/ LuaValue](Evalsha(sha1, keys, args, Return(_)))

  def scriptexists[F[_]: Functor](scripts: NonEmptyList[String])(implicit I: Inject[ScriptAlgebra, F]): Free[F, NonEmptyList[Boolean]] =
    inject[F, ScriptAlgebra, NonEmptyList[Boolean]](Scriptexists(scripts, Return(_)))

  def scriptflush[F[_]: Functor](implicit I: Inject[ScriptAlgebra, F]): Free[F, Unit] =
    inject[F, ScriptAlgebra, Unit](Scriptflush(Return(())))

  def scriptkill[F[_]: Functor](implicit I: Inject[ScriptAlgebra, F]): Free[F, Unit] =
    inject[F, ScriptAlgebra, Unit](Scriptkill(Return(())))

  def scriptload[F[_]: Functor](script: String)(implicit I: Inject[ScriptAlgebra, F]): Free[F, String] =
    inject[F, ScriptAlgebra, String](Scriptload(script, Return(_)))
}

object ScriptAlgebra extends ScriptTypes with ScriptInstances with ScriptFunctions
