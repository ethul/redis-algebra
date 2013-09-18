package redis
package algebra

import scalaz.{-\/, \/-, Coproduct, Free, Functor}

import all._, Interpreter._

sealed abstract class Interpreter[F[_] : Functor] {
  def runAlgebra[A](algebra: F[Mem => (A, Mem)], mem: Mem): (A, Mem)
}

sealed trait InterpreterInstances {
  implicit val connectionAlgebraInterpreter: Interpreter[ConnectionAlgebra] =
    new Interpreter[ConnectionAlgebra] {
      def runAlgebra[A](algebra: ConnectionAlgebra[Mem => (A, Mem)], mem: Mem) =
        algebra match {
          case _ => ???
        }
    }
  implicit val hashAlgebraInterpreter: Interpreter[HashAlgebra] =
    new Interpreter[HashAlgebra] {
      def runAlgebra[A](algebra: HashAlgebra[Mem => (A, Mem)], mem: Mem) =
        algebra match {
          case _ => ???
        }
    }

  implicit val keyAgebraInterpreter: Interpreter[KeyAlgebra] =
    new Interpreter[KeyAlgebra] {
      def runAlgebra[A](algebra: KeyAlgebra[Mem => (A, Mem)], mem: Mem) =
        algebra match {
          case Del(k, h) =>
            val (b, c) = k.list.foldLeft((0, mem)) {
              case ((b,c), a) =>
                if (c.contains(a)) (b + 1, c - a) else (b, c)
            }
            h(b)(c)
          case Dump(k, h) => h(mem.get(k).map(_.hashCode.toString))(mem)
          case Exists(k, h) => h(mem.contains(k))(mem)
          case _ => ???
        }
    }

  implicit val listAlgebraInterpreter: Interpreter[ListAlgebra] =
    new Interpreter[ListAlgebra] {
      def runAlgebra[A](algebra: ListAlgebra[Mem => (A, Mem)], mem: Mem) =
        algebra match {
          case _ => ???
        }
    }

  implicit val scriptAlgebraInterpreter: Interpreter[ScriptAlgebra] =
    new Interpreter[ScriptAlgebra] {
      def runAlgebra[A](algebra: ScriptAlgebra[Mem => (A, Mem)], mem: Mem) =
        algebra match {
          case _ => ???
        }
    }

  implicit val serverAlgebraInterpreter: Interpreter[ServerAlgebra] =
    new Interpreter[ServerAlgebra] {
      def runAlgebra[A](algebra: ServerAlgebra[Mem => (A, Mem)], mem: Mem) =
        algebra match {
          case _ => ???
        }
    }

  implicit val setAlgebraInterpreter: Interpreter[SetAlgebra] =
    new Interpreter[SetAlgebra] {
      def runAlgebra[A](algebra: SetAlgebra[Mem => (A, Mem)], mem: Mem) =
        algebra match {
          case _ => ???
        }
    }

  implicit val stringAlgebraInterpreter: Interpreter[StringAlgebra] =
    new Interpreter[StringAlgebra] {
      def runAlgebra[A](algebra: StringAlgebra[Mem => (A, Mem)], mem: Mem) =
        algebra match {
          case _ => ???
        }
    }

  implicit val zsetAlgebraInterpreter: Interpreter[ZSetAlgebra] =
    new Interpreter[ZSetAlgebra] {
      def runAlgebra[A](algebra: ZSetAlgebra[Mem => (A, Mem)], mem: Mem) =
        algebra match {
          case _ => ???
        }
    }

  implicit def coproductAlgebraInterpreter[F[_]: Interpreter: Functor, G[_]: Interpreter: Functor]:
    Interpreter[({ type l[a] = Coproduct[F, G, a] })#l] = {
    type H[A] = Coproduct[F, G, A]
    new Interpreter[H] {
      def runAlgebra[A](algebra: H[Mem => (A, Mem)], mem: Mem) =
        algebra.run match {
          case -\/(fa) => implicitly[Interpreter[F]].runAlgebra(fa, mem)
          case \/-(fa) => implicitly[Interpreter[G]].runAlgebra(fa, mem)
        }
    }
  }
}

sealed trait InterpreterFunctions {
  type Mem = Map[String, String]

  def run[A](algebra: Free[R, A], mem: Mem): A =
    algebra.resume.fold({ (a: R[Free[R, A]]) =>
      val (x, y) = implicitly[Interpreter[R]].runAlgebra(a.map(a => (mem: Mem) => (a, mem)), mem)
      run(x, y)
    }, a => a)
}

object Interpreter extends InterpreterInstances with InterpreterFunctions
